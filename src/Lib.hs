{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE Arrows, NoMonomorphismRestriction #-}
{-# LANGUAGE DeriveGeneric #-}

module Lib where

import Web.Spock
import Web.Spock.Config

import Data.Aeson hiding (json)
import Data.Monoid ((<>))
import Data.Text (Text, pack)
import GHC.Generics
import Text.XML.HXT.Core
import Text.Pretty.Simple (pPrint) 

-- Kanji Data Type --
data Kanji = Kanji {
    literal :: Text, -- literal character
    grade :: Int, -- grade of the kanji
    strokes :: Int, -- strokes needed to write kanji
    jaOn :: [Text], -- "onyomi" of the kanji
    jaKun :: [Text], -- "kunyomi" of the kanji
    def :: [Text], -- definitions of the kanji
    nanori :: [Text] -- readings of the kanji used in names
} deriving (Generic, Show)

instance ToJSON Kanji

instance FromJSON Kanji

parseXML file = readDocument [ withValidate no, 
                               withRemoveWS yes  -- throw away formating WS
                             ] file

test = do 
    kanjis <- runX (parseXML "kanjidic2.xml" >>> getKanjis)
    pPrint $ take 100 $ kanjis

atTag tag = deep (isElem >>> hasName tag)

atAttrVal a v = deep (isElem >>> hasAttrValue a (\x -> x == v))

content = getChildren >>> getText

getKanjis = atTag "kanjidic2" >>>
    proc root -> do
        char <- atTag "character" -< root 
        literal <- content <<< atTag "literal" -< char
        grade <- withDefault (content <<< atTag "grade") "0" -< char
        strokes <- withDefault (content <<< atTag "stroke_count") "0" -< char
        jaOn <- listA (content <<< atAttrVal "r_type" "ja_on") -< char
        jaKun <- listA (content <<< atAttrVal "r_type" "ja_kun") -< char
        def <- listA (content <<< atTag "meaning") -< char
        nanori <- listA (content <<< atTag "nanori") -< char
        returnA -< Kanji {
            literal = pack literal,
            grade = read grade,
            strokes = read strokes,
            jaOn = pack <$> jaOn,
            jaKun = pack <$> jaKun,
            def = pack <$> def,
            nanori = pack <$> nanori
        }


