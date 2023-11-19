{-# LANGUAGE DeriveGeneric #-}

import GHC.Generics
import Data.Aeson
import qualified Data.ByteString.Lazy as B
import qualified Data.Map as Map
import Data.Map
import Data.Char
import Prelude
import Debug.Trace

-- Definir la estructura de datos que representa el formato del archivo JSON
data Reviews = Reviews { 
    reviewerID :: String,
    reviewText :: String,
    overall :: String
} deriving (Show, Generic)

data CleanData = CleanData{
    reviewerIDs :: String,
    reviewTexts :: [String],
    overalls :: String
} deriving (Show, Generic)

instance FromJSON Reviews

-- Get the text part of the reviews
getreviewText :: Reviews -> String
getreviewText (Reviews{reviewerID = _ , reviewText = n, overall = _}) = n

getoverall :: Reviews -> String
getoverall (Reviews{reviewerID = _ , reviewText = _, overall = m}) = m

-- copy the all the aspects of Reviews to CleanData and receive a list of string as reviewText and will be put into CleanData
copyReviewsToCleanData :: Reviews -> [String] -> CleanData
copyReviewsToCleanData (Reviews{reviewerID = n, reviewText = _, overall = m}) reviewTexts = CleanData{reviewerIDs= n, reviewTexts = reviewTexts,overalls =  m}

--Convert to lowercase
toLower2 :: String -> String
toLower2 = Prelude.map Data.Char.toLower

prepositions :: [String]
prepositions = ["a", "an", "the", "in", "on", "at", "by", "for", "with", "about", "against", "between", "into", "through", "during", "before", "after", "above", "below", "to", "from", "up", "down", "of", "over", "under","is","you","it", "but", "and", "as","are","is","my"]

--Tokenize each text
tokenizeEachText :: String -> [String]
tokenizeEachText = Prelude.words

--Delete non-alphabetic characters except spaces
deleteNonAlphaExceptSpace :: String -> String
deleteNonAlphaExceptSpace = Prelude.filter (\x -> Data.Char.isAlpha x || x == ' ')

--Delete frequent and unnecesary prepositions
deleteFrequentAndUnnecessaryWords :: String -> String
deleteFrequentAndUnnecessaryWords = unwords . Prelude.filter (`notElem` prepositions) . words . Prelude.map toLower -- tokenize each word and then compare every one of them with the prepositions list

--function that receives tokens and returns a list with each token without repetition
createMap :: [String] -> [String]
createMap [] = []
createMap (x:xs) = x : createMap (Prelude.filter (/= x) xs)


-- function that match every one of the overalls etiquette with the perspective token
matchOverallsWithTokens :: [String] -> [[String]] -> [(String, [String])]
matchOverallsWithTokens [] [] = []
matchOverallsWithTokens (x:xs) (y:ys) = (x,y) : matchOverallsWithTokens xs ys

-- function that receives a list of tuples and returns a map with the frequency of each token in every etiquette
storeAllWordsInEachEtiquette :: [(String, [String])] -> Map String [String]
storeAllWordsInEachEtiquette [] = Map.empty
storeAllWordsInEachEtiquette ((x, y):xs)
    | Map.member x restOfMap = Map.insert x (y ++ restOfMap ! x) restOfMap -- ! look the value asocciated with the key
    | otherwise = Map.insert x y restOfMap
  where
    restOfMap = storeAllWordsInEachEtiquette xs

--get the frequency of each token in every etiquette
getFrequencyOfEachTokenInEachEtiquette :: Map String [String] -> Map String (Map String Int)
getFrequencyOfEachTokenInEachEtiquette = Map.map (Map.fromListWith (+) . Prelude.map (,1)) -- it converts each word with its frequency in a tuple and then it adds the frequency of the same word

-- get the total frequency of each token
getFrequencyOfEachTokenInTotal :: Map String (Map String Int) -> Map String Int
getFrequencyOfEachTokenInTotal = Map.unionsWith (+) . Map.elems -- it converts each word with its frequency in a tuple and then it adds the frequency of the same word:
  
-- calculate the probability of each token in each etiquette and store it in a map
getFrequencyOfEachTokenInRelationWithTheTotal :: Map String (Map String Int) -> Map String Int -> Map String (Map String Float)
getFrequencyOfEachTokenInRelationWithTheTotal map1 map2 = Map.map (\v -> Map.mapWithKey (\k x -> fromIntegral x / fromIntegral (map2 ! k)) v) map1

-- read a comment from the user and return the etiquette
readComment :: IO String
readComment = do
    putStrLn "Enter a comment: "
    comment <- getLine
    return comment

-- get the probability of each token from tokens3 in each etiquette
getProbabilityOfEachTokenInEachEtiquette :: [String] -> Map String (Map String Float) -> Map String (Map String Float)
getProbabilityOfEachTokenInEachEtiquette array map = Map.map (\v -> Map.filterWithKey (\k x -> k `elem` array) v) map

-- calculate the total probability inside of each etiquette
getTotalProbabilityOfEachEtiquette :: Map String (Map String Float) -> Map String Float
getTotalProbabilityOfEachEtiquette map = Map.map (\v -> Map.foldr (+) 0 v) map


getEtiquetteWithTheHighestProbability :: Map String Float -> String
getEtiquetteWithTheHighestProbability probabilitiesMap =
    fst (Map.foldrWithKey findMaxProbability ("", -1.0/0) probabilitiesMap)

findMaxProbability :: String -> Float -> (String, Float) -> (String, Float)
findMaxProbability key probability (maxKey, maxProbability)
    | probability > maxProbability = (key, probability)
    | otherwise = (maxKey, maxProbability)


main :: IO()
main = do
    jsonData <- B.readFile "reviews_Video_GamesFinal.json"
    
    case decode jsonData :: Maybe [Reviews] of
        Just reviews -> do
            -- Get just the text part of the reviews
            let reviewTexts  = Prelude.map getreviewText reviews
            -- Get just the overall part of the reviews
            let overallsEtiquete = Prelude.map getoverall reviews
            -- Convert to lowercase
            let minus = Prelude.map toLower2 reviewTexts
            -- Delete non-alphabetic characters
            let alpha = Prelude.map deleteNonAlphaExceptSpace minus
            -- Delete frequent and unnecesary prepositions
            let datos = Prelude.map deleteFrequentAndUnnecessaryWords alpha
            -- Tokenize each text
            let tokens = Prelude.map tokenizeEachText datos
            -- receives tokens and returns a map with the frequency of each token
            let maps = Prelude.map createMap tokens
            -- match every one of the overalls etiquette with its respective token list
            let match = matchOverallsWithTokens overallsEtiquete maps
            -- store the words in each etiquette
            let store = storeAllWordsInEachEtiquette match
            -- get the frequency of each token in each etiquette
            let store2 = getFrequencyOfEachTokenInEachEtiquette store
            -- get the total frequency of each token
            let store3 = getFrequencyOfEachTokenInTotal store2
            -- calculate the probability of each token in each etiquette and store it in a map
            let store4 = getFrequencyOfEachTokenInRelationWithTheTotal store2 store3
            -- read a comment from the user and return the etiquette
            comment <- readComment
            -- Convert to lowercase
            let minus2 = toLower2 comment
            -- Delete non-alphabetic characters
            let alpha2 = deleteNonAlphaExceptSpace minus2
            -- Delete frequent and unnecesary prepositions
            let datos2 = deleteFrequentAndUnnecessaryWords alpha2
            -- Tokenize each text
            let tokens2 = tokenizeEachText datos2
            -- eliminate duplicate words
            let tokens3 = createMap tokens2
            -- get the probability of each token from tokens3 in each etiquette
            let tokens4 = getProbabilityOfEachTokenInEachEtiquette tokens3 store4
            -- calculate the total probability inside of each etiquette
            let tokens5 = getTotalProbabilityOfEachEtiquette tokens4

            -- get the etiquette with the highest probability and print it
            let max = getEtiquetteWithTheHighestProbability tokens5
        
            print max
            
        Nothing -> putStrLn "Error"