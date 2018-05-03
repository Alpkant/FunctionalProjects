import Prelude hiding (Word)
import Data.Char
import Data.List
import qualified Data.Map as M

type Word       = String
type Sentence   = [Word]
type CharCounts = M.Map Char Int

--wordCharCounts "all" gives result fromList [('a',1),('l',2)]
--function gives map with sorted according to the keys
wordCharCounts :: Word -> CharCounts
wordCharCounts = M.fromList . map (\x -> (head x,length x)) . group . sort . toLowerWord
    where
      toLowerWord ::  Word -> Word
      toLowerWord  = map toLower

--sentenceCharCounts ["all","hi"] gives result fromList [('a',1),('h',1),('i',1),('l',2)]
sentenceCharCounts :: Sentence -> CharCounts
sentenceCharCounts [x]      = wordCharCounts x
sentenceCharCounts (x:xs)   = M.unionWith (+) (wordCharCounts x)  (sentenceCharCounts xs)

--dictCharCounts ["all","hi"] gives result fromList [("all",fromList [('a',1),('l',2)]),("hi",fromList [('h',1),('i',1)])]
dictCharCounts :: Sentence -> M.Map Word CharCounts
dictCharCounts = M.fromList . map (\x -> (x,wordCharCounts x))

--dictWordsByCharCounts $ dictCharCounts ["all","hi"] gives result fromList [(fromList [('a',1),('l',2)],["all"]),(fromList [('h',1),('i',1)],["hi"])]
dictWordsByCharCounts :: M.Map Word CharCounts -> M.Map CharCounts Sentence
dictWordsByCharCounts ms = iterateKeys $ M.toList ms
    where
      iterateKeys :: [(Word,CharCounts)] -> M.Map CharCounts Sentence
      iterateKeys []     = M.fromList []
      iterateKeys (x:xs) = M.insertWith (++) (snd x) [(fst x)] $ iterateKeys xs

--wordAnagrams "hi" $ dictWordsByCharCounts $ dictCharCounts ["all","hi"] gives result ["hi"]
--if given word cannot be found in second argument then it will return []
wordAnagrams :: Word -> M.Map CharCounts Sentence -> Sentence
wordAnagrams key x = [z | (n,y) <- M.toList x , z <- y , n == wordCharCounts key ]


--This function is wrong currently. It gives unnecessary tuples.
charCountsSubsets :: CharCounts -> [CharCounts]
charCountsSubsets xs = iterateKeys $ powerset $ convertCounts $ M.toList xs
    where
      convertCounts :: [(Char,Int)] -> [(Char,Int)]
      convertCounts list = [(x,1) | (x,xs) <- list , y <- [1..xs]]

      powerset :: [a] ->  [[a]]
      powerset [] = [[]]
      powerset (x:xs) = [r | ps<-powerset xs, r<-[(x:ps),ps]]

      iterateKeys :: [[(Char,Int)]] -> [CharCounts]
      iterateKeys []     = [M.fromList[]]
      iterateKeys (x:xs) =  (simplyfCounts x) : (iterateKeys xs)
          where
            simplyfCounts :: [(Char,Int)] -> CharCounts
            simplyfCounts []   =  M.fromList []
            simplyfCounts (a:as) = M.insertWith (+) (fst a) (snd a) $ simplyfCounts as

-- subtractCounts  (wordCharCounts "alleee")  (wordCharCounts "al")
-- will give you fromList [('e',3),('l',1)]
-- Second argument always have to be subset of the first argument
subtractCounts :: CharCounts -> CharCounts -> CharCounts
subtractCounts fstMap sndMap = M.filter (> 0) $ traverseMap $ M.toList sndMap
    where
      traverseMap :: [(Char,Int)] -> CharCounts
      traverseMap [x] = M.adjustWithKey (\key a -> a-1) (fst x) fstMap
      traverseMap (x:xs) = M.adjustWithKey (\key a -> a-1) (fst x) $ traverseMap xs
