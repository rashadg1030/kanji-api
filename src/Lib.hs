{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE Arrows, NoMonomorphismRestriction #-}

module Lib where

--import Data.Aeson hiding (print)
import Data.Text as T hiding (length, take)
import Text.XML.HXT.Core
import Text.Pretty.Simple (pPrint) 

-- Kanji Data Type --
data Kanji = Kanji {
    literal :: T.Text, -- literal character
    grade :: Int, -- grade of the kanji
    strokes :: Int, -- strokes needed to write kanji
    jaOn :: [T.Text], -- "onyomi" of the kanji
    jaKun :: [T.Text], -- "kunyomi" of the kanji
    def :: [T.Text], -- definitions of the kanji
    nanori :: [T.Text] -- readings of the kanji used in names
} deriving (Show)

parseXML file = readDocument [ withValidate no, 
                               withRemoveWS yes  -- throw away formating WS
                             ] file

test = do 
    kanjis <- runX (parseXML "kanjidic2.xml" >>> getKanjis)
    pPrint $ take 100 $ kanjis

atTag tag = deep (isElem >>> hasName tag)

atAttrVal a v = deep (isElem >>> hasAttrValue a (\x -> x == v))

text = getChildren >>> getText

getKanjis = atTag "kanjidic2" >>>
    proc root -> do
        char <- atTag "character" -< root 
        literal <- text <<< atTag "literal" -< char
        grade <- withDefault (text <<< atTag "grade") "0" -< char
        strokes <- withDefault (text <<< atTag "stroke_count") "0" -< char
        jaOn <- listA (text <<< atAttrVal "r_type" "ja_on") -< char
        jaKun <- listA (text <<< atAttrVal "r_type" "ja_kun") -< char
        def <- listA (text <<< atTag "meaning") -< char
        nanori <- listA (text <<< atTag "nanori") -< char
        returnA -< Kanji {
            literal = T.pack literal,
            grade = read grade,
            strokes = read strokes,
            jaOn = T.pack <$> jaOn,
            jaKun = T.pack <$> jaKun,
            def = T.pack <$> def,
            nanori = T.pack <$> nanori
        }


