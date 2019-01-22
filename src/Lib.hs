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
   
import Data.Time (UTCTime)
import Servant.API
import Data.Aeson hiding (json)
import Data.Monoid ((<>))
import Data.Text (Text, pack, unpack)
import GHC.Generics
import Control.Monad.IO.Class
import Database.PostgreSQL.Simple

type KanjiAPI = "kanjis" :> Get '[JSON] [Kanji]

-- Kanji Data Type --
data Kanji = Kanji {
    literal :: Text, -- literal character
    grade :: Int, -- grade of the kanji
    strokes :: Int, -- strokes needed to write kanji
    jaOn :: Text, -- "onyomi" of the kanji
    jaKun :: Text, -- "kunyomi" of the kanji
    def :: Text, -- definitions of the kanji
    nanori :: Text -- readings of the kanji used in names
} deriving (Generic, Show)

instance ToJSON Kanji where
    toJSON Kanji{..} = object [ "literal" .= literal,
                                "grade" .= grade,
                                "strokes" .= strokes,
                                "jaOn" .= (strip jaOn),
                                "jaKun" .= (strip jaKun),
                                "def" .= (strip def),
                                "nanori" .= (strip nanori)
                              ]

instance FromJSON Kanji where 
    parseJSON = withObject "Kanji" $ \o -> do 
        literal <- o .: "literal"
        grade <- o .: "grade"
        strokes <- o .: "strokes"
        jaOn <- bling <$> o .: "jaOn"
        jaKun <- bling <$> o .: "jaKun"
        def <- bling <$> o .: "def"
        nanori <- bling <$> o .: "nanori"
        return Kanji{..} 

-- Get Kanjis from database
getKanjis :: IO ()
getKanjis = do
    secret <- readFile "secret.txt"
    conn <- connect defaultConnectInfo {
        connectHost = "kanjidb.postgres.database.azure.com",
        connectUser = "rashadg1030@kanjidb",
        connectPassword = secret,
        connectDatabase = "kanjidb"
    }
    return ()




-- Helper Functions --
bling :: [Text] -> Text
bling []     = ""
bling (x:xs) = x <> "|" <> bling xs

strip :: Text -> [Text]
strip = (pack <$>) . (splitWhere '|') . unpack
        where
            splitWhere :: Char -> String -> [String]
            splitWhere _ "" = []
            splitWhere c s = first' $ gop c ([], "", s)

first' :: (a, b, c) -> a
first' (x, _, _) = x

gop :: Char -> ([String], String, String) -> ([String], String, String)
gop c (words, acc, "")           = (words, "", "")
gop c (words, acc, next@(h2:t2)) = if c == h2 then gop c (words ++ [acc], "", t2) else gop c (words, acc ++ [h2], t2)

--ex1 :: Kanji

