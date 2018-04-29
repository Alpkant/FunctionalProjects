import Prelude hiding (Word)
import Data.Char
import Data.List
type Word       = String
type Sentence   = [Word]
type CharCounts = [(Char, Int)]

wordCharCounts :: Word -> CharCounts
wordCharCounts = map (\x -> (head x,length x)) . group . sort . toLowerWord
    where
      toLowerWord ::  Word -> Word
      toLowerWord  = map toLower

sentenceCharCounts :: Sentence -> CharCounts
sentenceCharCounts [x]      = wordCharCounts x
sentenceCharCounts (x:xs)   = (wordCharCounts x) ++ (sentenceCharCounts xs)

dictCharCounts :: Sentence -> [(Word,CharCounts)]
dictCharCounts = map (\x -> (x,wordCharCounts x))
