import qualified Data.Map as M
import Data.Maybe
import Data.Char
import qualified Data.List as L
import System.Environment
import System.IO
import Prelude hiding (Word)

data Trie = Trie {end :: Bool, children :: M.Map Char Trie}
            deriving Show

data Action = Add  Word | Search Word | Prefix Word | Print | Exit
            deriving (Eq,Show)

type Word = String

empty :: Trie
empty = Trie {end = False , children = M.empty }

insert :: Word -> Trie -> Trie
insert [] (Trie e t)        = Trie {end = True , children = t } --End char will have True for end
insert x'@(x:xs) (Trie e t) = case M.lookup x t of
                              Just value -> Trie {end = e , children = M.insert x (insert xs value) t }
                              Nothing    -> Trie {end = e , children = M.insert x (insert xs empty) t }

insertList :: [Word] -> Trie
insertList = foldr insert empty

search :: Word -> Trie -> Bool
search []     (Trie e _)  = e
search (x:xs) (Trie _ t) = case M.lookup x t of
                           Just value -> search xs value
                           Nothing    -> False

getWords :: Trie -> [Word]
getWords t = getWords' t []
    where
        getWords' :: Trie -> [Word] -> [Word]
        getWords' (Trie e t) xs
            | null listOfChildren = xs
            | e                   = (head xs) : traverseChildTries
            | otherwise           = traverseChildTries
            where
                listOfChildren:: [(Char,Trie)]
                listOfChildren = M.toList t

                traverseChildTries:: [Word]
                traverseChildTries = concat $ map (\(x,y) -> getWords' y (concatr x xs) ) listOfChildren

        concatr :: Char -> [Word] -> [Word]
        concatr x [] = [[x]]
        concatr x ys = map (\a -> a ++ [x]) ys

prefix :: Word -> Trie -> Maybe [Word]
prefix w xs = makeMaybeList allPrefix
    where
        -- Filter all the words that matches with prefix word
        allPrefix::[Word]
        allPrefix = [x | x <- allWords , L.isPrefixOf w x]
        -- Get all words of the Trie
        allWords::[Word]
        allWords  = getWords xs

        -- Make the prefix list Maybe List.
        -- It returns Nothing if no matched prefix in Trie
        makeMaybeList:: [Word] -> Maybe [Word]
        makeMaybeList [] = Nothing
        makeMaybeList m  = Just m
