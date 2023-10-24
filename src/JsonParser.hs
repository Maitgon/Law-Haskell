{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
module JsonParser where

import GHC.Generics ( Generic )
import Data.Aeson ( decode, (.:), withObject, FromJSON(parseJSON) )
import qualified Data.ByteString.Lazy as B
import Data.Maybe ( fromJust )
import qualified Data.Map as M

data Datos = Datos {
      informacion :: Informacion
    , totales  :: Totales
    , votaciones :: [Votaciones]
} deriving (Generic, Show)

instance FromJSON Datos where
    parseJSON = withObject "Law" $ \v -> Datos
        <$> v .: "informacion"
        <*> v .: "totales"
        <*> v .: "votaciones"

data Informacion = Informacion {
      sesion :: Int
    , numeroVotacion :: Int
    , fecha :: String
    , titulo :: String
    , textoExpediente :: String
    , tituloSubGrupo :: String
    , textoSubGrupo :: String
    , votacionesConjuntas :: [Int]
} deriving (Generic, Show)

instance FromJSON Informacion where
    parseJSON = withObject "informacion" $ \v -> Informacion
        <$> v .: "sesion"
        <*> v .: "numeroVotacion"
        <*> v .: "fecha"
        <*> v .: "titulo"
        <*> v .: "textoExpediente"
        <*> v .: "tituloSubGrupo"
        <*> v .: "textoSubGrupo"
        <*> v .: "votacionesConjuntas"

data Totales = Totales {
      asentimiento :: String
    , presentes :: Int
    , afavor :: Int
    , enContra :: Int
    , abstenciones :: Int
    , noVotan :: Int
} deriving (Generic, Show)

instance FromJSON Totales where
    parseJSON = withObject "totales" $ \v -> Totales
        <$> v .: "asentimiento"
        <*> v .: "presentes"
        <*> v .: "afavor"
        <*> v .: "enContra"
        <*> v .: "abstenciones"
        <*> v .: "noVotan"

data Votaciones = Votaciones {
      asiento :: String
    , diputado :: String
    , grupo :: String
    , voto :: String
} deriving (Generic, Show)

instance FromJSON Votaciones where
    parseJSON = withObject "votaciones" $ \v -> Votaciones
        <$> v .: "asiento"
        <*> v .: "diputado"
        <*> v .: "grupo"
        <*> v .: "voto"

type Partido = String

data Voto = Favor | Abstiene | Contra
    deriving (Eq, Show)


getVotings :: B.ByteString -> M.Map Partido Voto
getVotings byteString = M.delete "GMx" $ M.delete "GPlu" todos
    where parsed = decode byteString
          votos  = votaciones $ fromJust parsed
          todos  = voting $ zip (map grupo votos) (map voto votos)

voting :: [(Partido, String)] -> M.Map Partido Voto
voting votacion = M.map decide $ M.fromListWith (+) $ map aux votacion
    where aux :: (Partido, String) -> (Partido, Int)
          aux (x, "S\237") = (x, 1)
          aux (x, "No") = (x, -1)
          aux (x, _) = (x, 0)
          decide x
            | x < -3 = Contra
            | x > 3 = Favor
            | otherwise = Abstiene

data Law = Law {
      numero :: String
    , nombre :: String
    , votos  :: M.Map Partido Voto
} deriving (Eq, Show)


more :: [Int] -> IO [Law]
more laws = do
    let byteStrings = [B.readFile $ "LAW_input/data/VOT" ++ show x ++ ".json" | x <- laws]
    votos <- mapM (fmap getVotings) byteStrings
    leyes <- mapM (fmap $ textoExpediente . informacion . fromJust . decode) byteStrings
    -- same has sequence $ (map . fmap) getVotings byteStrings
    let newNames = [ "c" ++ show x | x <- [1..length laws]]
    let sol = zipWith3 Law newNames leyes votos
    return sol

---------------------
-- Escribir en txt --
---------------------

start :: [Int] -> [String]
start xs = ["Input " ++ parsePC xs ++ ".\n\
            \\n\
            \Number of seats:\n\
            \350\n\
            \\n\
            \Parties:\n\
            \0 : GCUP-EC-GC\n\
            \1 : GCs\n\
            \2 : GEH Bildu\n\
            \3 : GP\n\
            \4 : GR\n\
            \5 : GS\n\
            \6 : GV\n\
            \7 : GVOX\n"]

prosContras :: [Law] -> ([[Int]], [[Int]])
prosContras law = (map pros votos1, map contras votos1)
    where votos1 = map (M.toList . votos) law


pros :: [(Partido, Voto)] -> [Int]
pros xs = prosAux xs 0

prosAux :: [(Partido, Voto)] -> Int -> [Int]
prosAux [] _         = []
prosAux ((x,y):xs) n = if y == Favor 
                    then n : prosAux xs (n+1)
                    else prosAux xs (n+1)

contras :: [(Partido, Voto)] -> [Int]
contras xs = contrasAux xs 0

contrasAux :: [(Partido, Voto)] -> Int -> [Int]
contrasAux [] _ = []
contrasAux ((x,y):xs) n = if y == Contra
                    then n : contrasAux xs (n+1)
                    else contrasAux xs (n+1)

parsePC :: [Int] -> String
parsePC [] = []
parsePC [x] = show x
parsePC (x:xs) = show x ++ "," ++ parsePC xs

main :: IO ()
main = do
    let laws1 = [1..27]
    sol <- more laws1
    --print sol

    let names1 = map nombre sol
    --print names1

    let nombres = "Laws:" : ["c" ++ show x ++ " : " ++ (names1 !! (x-1)) | x <- [1..length names1]]
    let nombres2 = map (filter (/= '\n')) nombres
    --putStrLn $ unlines nombres2

    --putStrLn $ unlines $ start ++ nombres2
    let (pro, contra) = prosContras sol
    let proStr = unlines $ "Pro:" : ["c" ++ show x ++ " : " ++ parsePC (pro !! (x-1)) | x <- [1..length pro]]
    --putStrLn proStr
    let contraStr = unlines $ "Against:" : ["c" ++ show x ++ " : " ++ parsePC (contra !! (x-1)) | x <- [1..length pro]]
    --putStrLn contraStr

    let final = unlines (start laws1 ++ nombres2) ++ "\n" ++ proStr ++ "\n" ++ contraStr
    putStrLn final
    writeFile "input6.txt" final


