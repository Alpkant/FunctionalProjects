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

getWord :: IO String
getWord = do
            putStrLn "\nEnter word/prefix:"
            hFlush stdout
            word <- getLine
            let lower = map toLower word
            return lower

doAction :: Action -> Trie -> IO Trie
doAction (Add word)     tree = do
                                putStrLn "New word is added!"
                                return $ insert word tree

doAction (Search word)  tree = do
                                if search word tree
                                then putStrLn "Exists in dictionary!"
                                else putStrLn "NOT exists!"
                                return tree

doAction (Prefix word)  tree = do
                                let list = prefix word tree
                                if  list == Nothing
                                then do putStrLn "No words found with that prefix!"
                                else do
                                    putStrLn "Found words:"
                                    putStrLn $ L.intercalate "\n" $ fromJust list --In order to print line by line
                                return tree

doAction  Print         tree = do
                                putStrLn "List of words in dictionary:"
                                putStrLn $ L.intercalate "\n" $ getWords tree --In order to print line by line
                                return tree


printMenu :: IO()
printMenu = do
            putStrLn "a) Add Word"
            putStrLn "s) Search Word"
            putStrLn "f) Find words with prefix"
            putStrLn "p) Print all words"
            putStrLn "e) Exit"
            putStrLn "Enter the action:"


doLoop :: Trie -> IO()
doLoop maintree = do
        printMenu
        choice <- getChar
        if choice == 'a'
        then do
            key  <- getWord
            tree <- doAction (Add key) maintree
            doLoop tree

        else if choice == 's'
        then do
            key <- getWord
            tree <- doAction (Search key) maintree
            doLoop tree

        else if choice == 'f'
        then do
            key <- getWord
            tree <- doAction (Prefix key) maintree
            doLoop tree
        else if choice == 'p'
        then do
            tree <- doAction Print maintree
            doLoop tree

        else if choice == 'e'
            then return()

        else doLoop maintree

main = do
    hSetBuffering stdin NoBuffering
    args <- getArgs
    contents <- readFile $ head args
    let dictionary = words $ contents
    let maintree = insertList dictionary
    doLoop maintree
