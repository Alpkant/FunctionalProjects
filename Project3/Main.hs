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

dictCharCounts :: Sentence -> [(Word,CharCounts)]
dictCharCounts = map (\x -> (x,wordCharCounts x))
