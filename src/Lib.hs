{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}

-- For Database
{-# LANGUAGE EmptyDataDecls             #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE QuasiQuotes                #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeFamilies               #-}

module Lib (app) where

import Web.Spock
import Web.Spock.Config
import Data.Aeson hiding (json)
import Data.Monoid ((<>))
import Data.Text (Text, pack, unpack)
import GHC.Generics
import Control.Monad.IO.Class

-- For Database
import Control.Monad.Logger (LoggingT, runStdoutLoggingT)
import Database.Persist hiding (get) -- To avoid a naming clash with Web.Spock.get
import qualified Database.Persist as P         -- We'll be using P.get later for GET /people/<id>.
import Database.Persist.Postgresql hiding (get)
import Database.Persist.TH

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

type Api = SpockM () () () ()

type ApiAction a = SpockAction () () () a

app :: Api
app = do
        get "kanji" $ do
           json . show $ "kanji"

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
