import Prelude hiding (Word)
import Data.Char
import Data.List
import qualified Data.Map as M

type Word       = String
type Sentence   = [Word]
type CharCounts = M.Map Char Int

wordCharCounts :: Word -> CharCounts
wordCharCounts = M.fromList . map (\x -> (head x,length x)) . group . sort . toLowerWord
    where
      toLowerWord ::  Word -> Word
      toLowerWord  = map toLower

sentenceCharCounts :: Sentence -> CharCounts
sentenceCharCounts [x]      = wordCharCounts x
sentenceCharCounts (x:xs)   = M.unionWith (+) (wordCharCounts x)  (sentenceCharCounts xs)

dictCharCounts :: Sentence -> M.Map Word CharCounts
dictCharCounts = M.fromList . map (\x -> (x,wordCharCounts x))

dictWordsByCharCounts :: M.Map Word CharCounts -> M.Map CharCounts Sentence
dictWordsByCharCounts ms = iterateKeys $ M.toList ms
    where
      iterateKeys :: [(Word,CharCounts)] -> M.Map CharCounts Sentence
      iterateKeys []     = M.fromList []
      iterateKeys (x:xs) = M.insertWith (++) (snd x) [(fst x)] $ iterateKeys xs

wordAnagrams :: Word -> M.Map CharCounts Sentence -> Sentence
wordAnagrams key x = [z | (n,y) <- M.toList x , z <- y , n == wordCharCounts key ]
