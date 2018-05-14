import qualified Data.Map as M
import Data.Maybe
import System.Environment
import System.IO
import Prelude hiding (Word)

data Trie = Trie {end :: Bool, children :: M.Map Char Trie}
            deriving Show
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
getWords = undefined

prefix :: Word -> Trie -> Maybe [Word]
prefix = undefined
