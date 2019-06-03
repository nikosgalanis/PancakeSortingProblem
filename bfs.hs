import Data.List
import Debug.Trace
import Functions
import Data.Function (on)
import qualified Data.Set as Set

------------------------------
---- BSF Sorting Function
------------------------------

-- generating successors of stack
-- searching for goal at leaf nodes before pushing them in frontier
-- returns 1 if goal was found - return path and exit
-- returns frontier and explored set in any case
substacks :: Int -> ([Int],[Int]) -> [Int] -> [([Int],[Int])] -> Set.Set [Int] -> ([([Int],[Int])],Set.Set [Int],Int)
substacks n stack goal frontier explored | x == goal = (fr,explored,1)
                                         | n > length(fst stack) = (frontier,explored,0)
                                         | (Set.member x explored == False) && (elem x (map fst frontier) == False) = substacks (n+1) stack goal fr explored
                                         | otherwise = substacks (n+1) stack goal frontier explored
                                         where x = flip_stack n (fst stack)
                                               p = (snd stack) ++ [n]
                                               fr = frontier ++ [(x,p)]

-- continue generating successors until goal is found
bfs_search :: [Int] -> [([Int],[Int])] -> Set.Set [Int] -> [Int]
bfs_search goal frontier explored | end == 1 = snd (last f)
                                  | otherwise = bfs_search goal fr s
                                  where (f,s,end) = substacks 2 element goal frontier set
                                        fr = tail f
                                        element = head frontier
                                        set = Set.insert (fst element) explored

bfs :: [Int] -> [Int]
bfs x | x == goal = []
      | otherwise = bfs_search goal frontier explored
        where frontier = [(x,[])]
              s = Set.empty
              explored = Set.insert x s
              goal = quicksort x


--batch implementation
many_stacks :: [[Int]] -> [[Int]] -> [[Int]] -> [[Int]]
many_stacks [] _ _ = []
many_stacks (h:l) single bfsed = return_nth n bfsed : many_stacks l single bfsed
                                 where n = return_index 1 h single

batch :: [[Int]] -> [[Int]]
batch l = many_stacks len single bfsed
          where bfsed = map bfs (single)
                single = remove_duplicates len
                len = list_of_lengthish l
