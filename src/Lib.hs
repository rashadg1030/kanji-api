{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}

module Lib where
   
import Control.Monad 
import Data.Time (UTCTime)
import Servant
import Servant.API 
import Servant.Server
import Data.Aeson hiding (json)
import Data.Aeson.Types
import Data.Monoid ((<>))
import Data.Text (Text, pack, unpack, takeWhile)
import Text.Pretty.Simple (pPrint)
import GHC.Generics
import Control.Monad.IO.Class
import Database.PostgreSQL.Simple
import Network.Wai
import Network.Wai.Handler.Warp
import GHC.TypeLits
import Prelude hiding (takeWhile)

-- Kanji Data Type --

data Kanji = Kanji {
    literal :: Text, 
    grade :: Int, 
    strokes :: Int, 
    jaon :: Text, 
    jakun :: Text,
    def :: Text, 
    nanori :: Text
} deriving (Generic, Show)

instance ToJSON Kanji where
    toJSON Kanji{..} = object [ "literal" .= literal,
                                "grade" .= grade,
                                "strokes" .= strokes,
                                "jaon" .= (deserialize jaon),
                                "jakun" .= (deserialize jakun),
                                "def" .= (deserialize def),
                                "nanori" .= (deserialize nanori)
                              ]

instance FromJSON Kanji where 
    parseJSON = withObject "Kanji" $ \o -> do 
        literal <- o .: "literal"
        grade <- o .: "grade"
        strokes <- o .: "strokes"
        jaon <- serialize <$> o .: "jaon"
        jakun <- serialize <$> o .: "jakun"
        def <- serialize <$> o .: "def"
        nanori <- serialize <$> o .: "nanori"
        return Kanji{..} 

instance ToRow Kanji
instance FromRow Kanji

-- get all kanji 
-- kanji/  

-- get kanji by id
-- kanji/:id

-- get kanji by jaon
-- kanji/yomi?on=<value>

-- get kanji by jakun
-- kanji/yomi?kun=<value>

-- get kanji by jaon and jakun
-- kanji/yomi?on=<value>&kun=<value>


type KanjiAPI = Endpoint1 :<|> Endpoint2 :<|> Endpoint3

type Endpoint1 = "kanji" :> Get '[JSON] [Text]
type Endpoint2 = "kanji" :> Capture "id" Int :> Get '[JSON] [Text]
type Endpoint3 = "kanji" :> "yomi" :> QueryParam "on" Text :> QueryParam "kun" Text :> Get '[JSON] Text
    
kanjiAPI :: Proxy KanjiAPI
kanjiAPI = Proxy

server :: Server KanjiAPI
server = getAllKanji :<|> getKanjiById :<|> getKanjiByYomi

-- Handlers --
getAllKanji :: Handler [Text]
getAllKanji = liftIO $ fetchLiteral

getKanjiById :: Int -> Handler [Text]
getKanjiById i = do
    conn <- liftIO connectDb
    literals <- liftIO $ query conn "select literal from testTbl where id=?" [i]
    return $ join literals

getKanjiByYomi :: Maybe Text -> Maybe Text -> Handler Text
getKanjiByYomi (Just jaon) Nothing      = return "jaonfilter"
getKanjiByYomi Nothing     (Just jakun) = return "jakunfilter" 
getKanjiByYomi (Just jaon) (Just jakun) = return "doublefilt"
getKanjiByYomi _ _                      = return "Nothing"

-- Main --
main' :: IO ()
main' = run 8080 app

app :: Application
app = serve kanjiAPI server

tst :: IO ()
tst = do 
    res <- fetchLiteral
    pPrint res

-- Get Kanjis from database
connectDb :: IO Connection
connectDb = do
    secret <- readFile "secret.txt"
    conn <- connect defaultConnectInfo {
        connectHost = "kanjidb.postgres.database.azure.com",
        connectUser = "rashadg1030@kanjidb",
        connectPassword = secret,
        connectDatabase = "kanjidb"
    }
    return conn  

fetchLiteral :: IO [Text] 
fetchLiteral = do
    conn <- connectDb
    literals <- query_ conn "select literal from testTbl"
    return $ join literals -- Must be double list because postgresql-simple can't infer the type or something

fetchDetail :: IO [Kanji]
fetchDetail = do
    conn <- connectDb
    details <- query_ conn "select literal, grade, strokes, jaon, jakun, def, nanori from testTbl"
    return details
            
hasJaOn :: Text -> Kanji -> Bool
hasJaOn t = elem t . (noOkuri <$>) . deserialize . jaon

hasJaKun :: Text -> Kanji -> Bool
hasJaKun t = elem t . (noOkuri <$>) . deserialize . jakun

noOkuri :: Text -> Text
noOkuri "" = ""
noOkuri t  = takeWhile ('.'/=) t

-- Helper Functions --
serialize :: [Text] -> Text
serialize []     = ""
serialize (x:xs) = x <> "|" <> serialize xs

deserialize :: Text -> [Text]
deserialize = (pack <$>) . groupAt . unpack
    where
        groupAt :: String -> [String]
        groupAt s = firstOfThree $ buildWordList '|' ([], "", s)

        firstOfThree :: (a, b, c) -> a
        firstOfThree (x, _, _) = x

        buildWordList :: Char -> ([String], String, String) -> ([String], String, String)
        buildWordList c (words, acc, "")           = (words, "", "")
        buildWordList c (words, acc, next@(h2:t2)) = if c == h2 then buildWordList c (words ++ [acc], "", t2) else buildWordList c (words, acc ++ [h2], t2)


