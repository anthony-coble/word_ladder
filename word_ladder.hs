import Control.Monad
import System.Environment (getArgs)
import Data.List
import Data.Maybe (fromJust)

main = do
  args <- getArgs
  dict <- getLines "four-char-dictionary.txt"
  findLadder args dict

getLines = liftM lines . readFile

data WordTree = WordTree String WordTree [WordTree]
              | Root
              deriving (Show, Read, Eq, Ord)

findLadder :: [String] -> [String] -> IO ()
findLadder (from:too:_)  dict = print $ concat $ intersperse "->" $ reverse $ pathTo $ fromJust $ findTree (buildTree from dict 10 [] Root) too 0
findLadder _ _ = print "Must provide two 4 letter words (e.g. word_ladder cute book)"

isOneAway :: String -> String -> Bool
isOneAway [a , b, c, d] [e, f, g, h] | a == e && b == f && c == g && d == h = False
                                     | a == e && b == f && c == g = True
                                     | a == e && b == f && d == h = True
                                     | a == e && c == g && d == h = True
                                     | b == f && c == g && d == h = True
                                     | otherwise = False

buildTree :: String -> [String] -> Integer -> [String] -> WordTree -> WordTree
buildTree start _ 0 _ parent = WordTree start parent []
buildTree start dict depth path parent  = WordTree start parent $ makeChildren start
  where getChildList word = filter (isOneAway word) dict \\ (start:path)
        makeChildren = map (makeChild) . getChildList
        makeChild current = buildTree current dict (depth - 1) (start:path) $ WordTree start parent []

getRow :: WordTree -> Integer -> [WordTree]
getRow tree@(WordTree vale parent children) depth | depth == 0 = tree:[]
                                                  | otherwise = concat $ map (\child -> getRow child (depth-1)) children

findTree:: WordTree -> String -> Integer -> Maybe WordTree
findTree tree target startLevel | null (getRow tree startLevel) = Nothing
                                | (length rowMatches) > 0 = Just $ head $ rowMatches
                                | otherwise = findTree tree target $ startLevel+1
                                  where rowMatches = filter (\(WordTree value _ _) -> value == target) (getRow tree startLevel)

pathTo :: WordTree -> [String]
pathTo tree@(WordTree value parent _) = value : (pathTo parent)
pathTo Root = []

