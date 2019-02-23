import Data.List
import Debug.Trace
import Data.Function (on)
import qualified Data.Set as Set

------------------------------
---- Several Functions
------------------------------

take_first :: (t,t) -> t
take_first (a,b) = a

take_second :: (t,t) -> t
take_second (a,b) = b

all_upside :: [(Int,Int)] -> [(Int,Int)]
all_upside [] = []
all_upside (h:l) = (take_first h,0) : all_upside l

flip_all :: [(Int,Int)] -> [(Int,Int)]
flip_all [] = []
flip_all ((f,s):l) | (s == 0) = (f,1) : flip_all l
                   | otherwise = (f,0) : flip_all l

flip_first :: [(Int,Int)] -> [(Int,Int)]
flip_first ((f,s): l) | (s == 0) = (f , 1) : l
                      | otherwise = (f , 0 ) :l

remove_duplicates :: (Ord a) => [a] -> [a]
remove_duplicates = map head . group . sort

--return the index of a list of lists
return_index :: Eq t => Int -> [t] -> [[t]] -> Int
return_index _ _ [] = -1
return_index n l (h:t) | l == h = n
                       | otherwise = return_index (n+1) l t

--those two functions split a list in 2 pieces
first_half n l = take n l
second_half n l = l \\ first_half n l
quicksort [] = []
quicksort (h:t) = (quicksort less) ++ [h] ++ quicksort greater
                  where less = filter ( < h) t
                        greater = filter ( >= h) t

position :: Eq a => a -> [a] -> Int
position i xs =
    case i `elemIndex` xs of
       Just n  -> n
       Nothing -> 0

indexOf :: Eq a => a -> [a] -> Int
indexOf n l = case elemIndex n l of
               Just n  -> n+1
               Nothing -> 0

--returns the nth biggest element of a list
nth_biggest :: Int -> [Int] -> Int
nth_biggest n l = return_nth n (reverse(quicksort l))


nth_biggest' :: Int -> [(Int,Int)] -> (Int,Int)
nth_biggest' n l = return_nth n (reverse(quicksort l))

return_nth :: Int -> [t] -> t
return_nth n l = l !! (n-1)

return_nth' :: Int -> [Int] -> Int
return_nth' _ [] = 0
return_nth' n (h:t)  | n == 1 = h
                    | otherwise = return_nth' (n-1) t

len :: [Int] -> [Int] -> [Int]
len [] _ = []
len (h:l) sorted = ((position h sorted) + 1) : (len l sorted)

lengthish :: [Int] -> [Int]
lengthish [] = []
lengthish l = (len l (quicksort l))

list_of_lengthish :: [[Int]] -> [[Int]]
list_of_lengthish [] = []
list_of_lengthish (h:l) = (len h (quicksort h)) : list_of_lengthish l

-- flips stack given a specific position
flip_stack :: Int -> [Int] -> [Int]
flip_stack pos s = (reverse (first_half pos s)) ++ second_half pos s

-- flips stack given a specific position
flip_stack' :: Int -> [(Int,Int)] -> [(Int,Int)]
flip_stack' pos s = (reverse z) ++ y
                  where x = first_half pos s
                        y = second_half pos s
                        z = map (\p@(f,o) -> if o == 1 then (f, 0) else (f, 1)) x

------------------------------
---- Naive Sorting Function
------------------------------

stupid_sort::[Int] -> [Int] -> Int -> [Int]
stupid_sort l goal n | (l /= goal) = index : ((length l) - n) : stupid_sort new_l goal (n+1)
                     | otherwise = []
                     where new_l = flip_stack ((length l) - n) temp
                           temp = flip_stack index l
                           index = (position nth l)+1
                           nth = nth_biggest (n+1) l

naive :: [Int] -> [Int]
naive l = filter (>1) (stupid_sort l goal 0)
          where goal = quicksort l

--visualize function
visualize::[Int] -> [Int] ->[[Int]]
visualize l [] = [l]
visualize l (h:m) = l : visualize new_l m
                    where new_l = flip_stack h l

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

--------------------------------------
----- Bidirectional Sorting Function
--------------------------------------

-- generating successors of intial stack and sorted stack at the same time
-- returns path when a successors is found in both frontiers
bs :: [([Int],[Int])] -> Set.Set [Int] -> [([Int],[Int])] -> Set.Set [Int] -> [Int]
bs frontier explored frontier' explored' | (elem (fst (head frontier')) (map fst frontier) == True) = (snd (frontier!!pos)) ++ reverse(snd (head frontier'))
                                         | (elem (fst (head frontier)) (map fst frontier') == True) = (snd (head frontier)) ++ reverse(snd (frontier'!!pos'))
                                         | otherwise = bs fr s fr' s'
                                         where (f,s,e) = substacks 2 element [] frontier set
                                               fr = (tail f) ++ [head f]
                                               element = head frontier
                                               set = Set.insert (fst element) explored
                                               (f',s',e') = substacks 2 (head frontier') [] frontier' set'
                                               fr' = (tail f') ++ [head f']
                                               element' = head frontier'
                                               set' = Set.insert (fst element') explored'
                                               pos = (position (fst (head frontier')) (map fst frontier))
                                               pos' = (position (fst (head frontier)) (map fst frontier'))

bidirectional :: [Int] -> [Int]
bidirectional x = bs frontier explored frontier' explored'
  where frontier = [(x,[])]
        s = Set.empty
        explored = Set.insert x s
        goal = quicksort x
        frontier' = [(goal,[])]
        s' = Set.empty
        explored' = Set.insert goal s'

------------------------------
---- Batch Sorting Function
------------------------------

many_stacks :: [[Int]] -> [[Int]] -> [[Int]] -> [[Int]]
many_stacks [] _ _ = []
many_stacks (h:l) single bfsed = return_nth n bfsed : many_stacks l single bfsed
                                 where n = return_index 1 h single

batch :: [[Int]] -> [[Int]]
batch l = many_stacks len single bfsed
          where bfsed = map bfs (single)
                single = remove_duplicates len
                len = list_of_lengthish l

------------------------------
---- Burned Pancakes
------------------------------

-- implementing same functions considering burned sides this time
-- burned sides should be downside
burnt_stupid_sort :: [(Int,Int)] -> [(Int,Int)] -> Int -> [Int]
burnt_stupid_sort l goal n | (l == goal) = []
                           | (take_second (head temp) == 0) = index : 1 : ((length l) - n) : burnt_stupid_sort new_l' goal (n+1)
                           | otherwise = index : ((length l) - n) : burnt_stupid_sort new_l goal (n+1)
                           where new_l = (flip_all (reverse new_first)) ++ new_second
                                 new_l' = (flip_all (reverse new_first')) ++ new_second'
                                 new_second = second_half ((length l) - n) temp
                                 new_first = first_half ((length l) - n) temp
                                 new_second' = second_half ((length l) - n) temp'
                                 new_first' = first_half ((length l) - n) temp'
                                 temp' = flip_first temp
                                 temp = (flip_all (reverse first)) ++ second
                                 second = second_half index l
                                 first = first_half index l
                                 index = ((position nth l) +1)
                                 nth = nth_biggest' (n+1) l

burnt_naive :: [(Int,Int)] -> [Int]
burnt_naive l = burnt_stupid_sort l goal 0
              where goal = all_upside (quicksort l)

burnt_visualize :: [(Int,Int)] -> [Int] ->[[(Int,Int)]]
burnt_visualize l [] = [l]
burnt_visualize l (h:m) = l : burnt_visualize new_l m
                          where new_l = (flip_all (reverse first)) ++ second
                                first = first_half h l
                                second = second_half h l

burnt_substacks :: Int -> ([(Int,Int)],[Int]) -> [(Int,Int)] -> [([(Int,Int)],[Int])] -> Set.Set [(Int,Int)] -> ([([(Int,Int)],[Int])],Set.Set [(Int,Int)],Int)
burnt_substacks n stack goal frontier explored | x == goal = (fr,explored,1)
                                               | n > length(fst stack) = (frontier,explored,0)
                                               | (Set.member x explored == False) && (elem x (map fst frontier) == False) = burnt_substacks (n+1) stack goal fr explored
                                               | otherwise = burnt_substacks (n+1) stack goal frontier explored
                                               where x = flip_stack' n (fst stack)
                                                     p = (snd stack) ++ [n]
                                                     fr = frontier ++ [(x,p)]

burnt_bfs_search :: [(Int,Int)] -> [([(Int,Int)],[Int])] -> Set.Set [(Int,Int)] -> [Int]
burnt_bfs_search goal frontier explored | end == 1 = snd (last f)
                                        | otherwise = burnt_bfs_search goal fr s
                                        where (f,s,end) = burnt_substacks 1 element goal frontier set
                                              fr = tail f
                                              element = head frontier
                                              set = Set.insert (fst element) explored

burnt_bfs :: [(Int,Int)] -> [Int]
burnt_bfs x | x == goal = []
            | otherwise = burnt_bfs_search goal frontier explored
            where
            frontier = [(x,[])]
            s = Set.empty
            explored = Set.insert x s
            y = reverse (sortBy (flip compare `on` fst) x)
            goal = map (\p@(f, o) -> if o == 1 then (f, 0) else p) y

burnt_bs :: [([(Int,Int)],[Int])] -> Set.Set [(Int,Int)] -> [([(Int,Int)],[Int])] -> Set.Set [(Int,Int)] -> [Int]
burnt_bs frontier explored frontier' explored' | (elem (fst (head frontier')) (map fst frontier) == True) = (snd (frontier!!pos)) ++ reverse(snd (head frontier'))
                                               | (elem (fst (head frontier)) (map fst frontier') == True) = (snd element) ++ reverse(snd (frontier'!!pos'))
                                               | otherwise = burnt_bs fr s fr' s'
                                               where (f,s,_) = burnt_substacks 1 element [] frontier set
                                                     fr = tail f ++ [head f]
                                                     element = head frontier
                                                     set = Set.insert (fst element) explored
                                                     (f',s',_) = burnt_substacks 2 element' [] frontier' set'
                                                     fr' = tail f' ++ [head f']
                                                     element' = head frontier'
                                                     set' = Set.insert (fst element') explored'
                                                     pos = (position (fst (head frontier')) (map fst frontier))
                                                     pos' = (position (fst (head frontier)) (map fst frontier'))

burnt_bidirectional :: [(Int,Int)] -> [Int]
burnt_bidirectional x = burnt_bs frontier explored frontier' explored'
                        where frontier = [(x,[])]
                              s = Set.empty
                              explored = Set.insert x s
                              y = reverse (sortBy (flip compare `on` fst) x)
                              goal = map (\p@(f, o) -> if o == 1 then (f, 0) else p) y
                              frontier' = [(goal,[])]
                              s' = Set.empty
                              explored' = Set.insert goal s'

burnt_len :: [(Int,Int)] -> [(Int,Int)] -> [(Int,Int)]
burnt_len [] _ = []
burnt_len ((f,s):l) sorted = (((position (f,s) sorted) +1) , s) : (burnt_len l sorted)

burnt_list_of_lengthish :: [[(Int,Int)]] -> [[(Int,Int)]]
burnt_list_of_lengthish [] = []
burnt_list_of_lengthish (h:l) = (burnt_len h (quicksort h)) : burnt_list_of_lengthish l

burnt_many_stacks :: [[(Int,Int)]] -> [[(Int,Int)]] -> [[Int]] -> [[Int]]
burnt_many_stacks [] _ _ = []
burnt_many_stacks (h:l) single bfsed = return_nth n bfsed : burnt_many_stacks l single bfsed
                                        where n = return_index 1 h single

burnt_batch :: [[(Int,Int)]] -> [[Int]]
burnt_batch l = burnt_many_stacks len single bfsed
          where bfsed = map burnt_bidirectional (single)
                single = remove_duplicates len
                len = burnt_list_of_lengthish l

is_an_adjacency :: Int -> Int -> Int -> Bool
is_an_adjacency a b l | (abs(a-b) == 1) || (a==1 && b==l) = True
                      | otherwise = False

free :: Int -> [Int] -> Bool
free _ [] = False
free x l | (ind == ll) = not (is_an_adjacency a x ll)
         | (ind == 1) = not (is_an_adjacency x b ll)
         | (ind == 0) = False
         | otherwise = (not(is_an_adjacency a x ll) && not(is_an_adjacency x b ll))
            where a = return_nth' (ind-1) l
                  b = return_nth' (ind+1) l
                  ind = indexOf x l
                  ll = length l

in_a_block :: Int -> [Int] -> Bool
in_a_block x l = not(free x l)

diff_blocks :: Int -> Int -> [Int] -> Bool
diff_blocks x y xs = abs(posx-posy) /= abs(x-y)
                     where posx = indexOf x xs -1
                           posy = indexOf y xs -1

first_of_block :: Int -> [Int] -> Bool
first_of_block x l | indexOf x l == 0 = False
                   | (prev == 0) = is_an_adjacency x next len
                   | otherwise = (not (is_an_adjacency prev x len) && is_an_adjacency x next len)
                   where prev = return_nth' ((indexOf x l) - 1) l
                         next = return_nth' ((indexOf x l) + 1) l
                         len = length(l)

last_of_block :: Int -> [Int] -> Bool
last_of_block x l | (next == 0) = is_an_adjacency prev x len
                  | otherwise = (is_an_adjacency prev x len && not(is_an_adjacency x next len))
                        where prev = return_nth' ((indexOf x l) - 1) l
                              next = return_nth' ((indexOf x l) + 1) l
                              len = length(l)

end_of_block :: [Int] -> Int -> Int
end_of_block [] _ = -1
end_of_block [x] _ = x
end_of_block (x:y:xs) len | (is_an_adjacency x y len == False) = x
                          | otherwise = end_of_block (y:xs) len

--t is free but t+1 and t-1
--are last elements of a block
flipping :: ([Int],[Int]) -> Int -> Int -> ([Int],[Int])
flipping list posx posp = (fs,ps)
                          where ys = flip_stack posx xs
                                posy = (indexOf x ys) -1
                                zs = flip_stack posy ys
                                posz = posp
                                vs = flip_stack posz zs
                                posv = (indexOf x vs) -1
                                xs = fst list
                                x = head xs
                                path = snd list
                                fs = flip_stack posv vs
                                ps = path ++ [posy,posz,posv]

----------------------------
-- Pancakes Sorting Problem
----------------------------

-- Cases Implementation

--t and t+o are both free
case_1a :: ([Int],[Int]) -> ([Int],[Int])
case_1a xs = (ys,p)
           where pos = (indexOf (x+1) list)-1
                 list = fst xs
                 path = snd xs
                 x = head list
                 p = path ++ [pos]
                 ys = flip_stack pos list

case_1b :: ([Int],[Int]) -> ([Int],[Int])
case_1b xs = (ys,p)
          where pos = (indexOf (x-1) list)-1
                list = fst xs
                path = snd xs
                x = head list
                p = path ++ [pos]
                ys = flip_stack pos list

--t is free and t+o is
--the 1st element of a block
case_2a :: ([Int],[Int]) -> ([Int],[Int])
case_2a xs = (ys,p)
           where pos = (indexOf (x+1) list)-1
                 list = fst xs
                 path = snd xs
                 x = head list
                 p = path ++ [pos]
                 ys = flip_stack pos list

case_2b :: ([Int],[Int]) -> ([Int],[Int])
case_2b xs = (ys,p)
         where pos = (indexOf (x-1) list)-1
               list = fst xs
               path = snd xs
               x = head list
               p = path ++ [pos]
               ys = flip_stack pos list

--t is free but both t+1 and t-1
--are last elements of blocks
case_3a :: ([Int],[Int]) -> ([Int],[Int])
case_3a xs | posn < posp = flipping xs posn posp
           | otherwise = flipping xs posp posn
           where posn = indexOf (x+1) list
                 posp = indexOf (x-1) list
                 list = fst xs
                 path = snd xs
                 x = head list

case_3b :: ([Int],[Int]) -> ([Int],[Int])
case_3b list = (zs,p)
             where posn = indexOf (x+1) xs
                   ys = flip_stack posn xs
                   posx = (indexOf x ys) -1
                   zs = flip_stack posx ys
                   xs = fst list
                   path = snd list
                   x = head xs
                   p = path ++ [posn,posx]

case_3c :: ([Int],[Int]) -> ([Int],[Int])
case_3c list = (zs,p)
            where posn = indexOf (x-1) xs
                  ys = flip_stack posn xs
                  posx = (indexOf x ys) -1
                  zs = flip_stack posx ys
                  xs = fst list
                  path = snd list
                  x = head xs
                  p = path ++ [posn,posx]

--t is in a block and t+o is free
case_4a :: ([Int],[Int]) -> ([Int],[Int])
case_4a xs = (ys,p)
           where pos = (indexOf (x+1) list)-1
                 list = fst xs
                 path = snd xs
                 x = head list
                 p = path ++ [pos]
                 ys = flip_stack pos list

case_4b :: ([Int],[Int]) -> ([Int],[Int])
case_4b xs = (ys,p)
        where pos = (indexOf (x-1) list)-1
              list = fst xs
              path = snd xs
              x = head list
              p = path ++ [pos]
              ys = flip_stack pos list

--t is free and t+o is
--the 1st element of a block
case_5a :: ([Int],[Int]) -> ([Int],[Int])
case_5a xs = (ys,p)
           where pos = (indexOf (x+1) list)-1
                 list = fst xs
                 path = snd xs
                 x = head list
                 p = path ++ [pos]
                 ys = flip_stack pos list

case_5b :: ([Int],[Int]) -> ([Int],[Int])
case_5b xs = (ys,p)
       where pos = (indexOf (x-1) list)-1
             list = fst xs
             path = snd xs
             x = head list
             p = path ++ [pos]
             ys = flip_stack pos list

--t is in a block with last element t+k*o
--t-o is in other block
--t+(k+1)*o is free
case_6a :: ([Int],[Int]) -> ([Int],[Int])
case_6a list = (fs,ps)
                where posx = indexOf ((end_of_block xs len) +1) xs
                      ys = flip_stack posx xs
                      posy = (indexOf ((end_of_block xs len)) ys) -1
                      zs = flip_stack posy ys
                      posz = (indexOf (x-1) zs)
                      vs = flip_stack posz zs
                      posi = (indexOf x vs) -1
                      xs = fst list
                      x = head xs
                      path = snd list
                      fs = flip_stack posi vs
                      ps = path ++ [posx,posy,posz,posi]
                      len = length(fst(list))

case_6a' :: ([Int],[Int]) -> ([Int],[Int])
case_6a' list = (fs,ps)
                where posx = indexOf ((end_of_block xs len) -1) xs
                      ys = flip_stack posx xs
                      posy = (indexOf ((end_of_block xs len)) ys) -1
                      zs = flip_stack posy ys
                      posz = (indexOf (x+1) zs)
                      vs = flip_stack posz zs
                      posi = (indexOf x vs) -1
                      xs = fst list
                      x = head xs
                      path = snd list
                      fs = flip_stack posi vs
                      ps = path ++ [posx,posy,posz,posi]
                      len = length(fst(list))

case_6b :: ([Int],[Int]) -> ([Int],[Int])
case_6b list = (fs,ps)
                where posx = indexOf ((end_of_block xs len) +1) xs
                      ys = flip_stack posx xs
                      posy = ((indexOf (end_of_block xs len) ys) -1)
                      zs = flip_stack posy ys
                      posz = indexOf x zs
                      vs = flip_stack posz zs
                      posi = (indexOf (x-1) vs) -1
                      xs = fst list
                      x = head xs
                      path = snd list
                      fs = flip_stack posi vs
                      ps = path ++ [posx,posy,posz,posi]
                      len = length(fst(list))

case_6b' :: ([Int],[Int]) -> ([Int],[Int])
case_6b' list = (fs,ps)
                where posx = indexOf ((end_of_block xs len) -1) xs
                      ys = flip_stack posx xs
                      posy = ((indexOf (end_of_block xs len) ys) -1)
                      zs = flip_stack posy ys
                      posz = indexOf x zs
                      vs = flip_stack posz zs
                      posi = (indexOf (x+1) vs) -1
                      xs = fst list
                      x = head xs
                      path = snd list
                      fs = flip_stack posi vs
                      ps = path ++ [posx,posy,posz,posi]
                      len = length(fst(list))

--t is in a block with last element t+k*o
--t-o is in other block
--t+(k+1)*o is in a block
case_7a :: ([Int],[Int]) -> ([Int],[Int])
case_7a list = (fs,ps)
                where posx = indexOf (end_of_block xs len) xs
                      ys = flip_stack posx xs
                      posy = ((indexOf ((end_of_block xs len) +1) ys) -1)
                      xs = fst list
                      x = head xs
                      path = snd list
                      fs = flip_stack posy ys
                      ps = path ++ [posx,posy]
                      len = length(fst(list))

case_7a' :: ([Int],[Int]) -> ([Int],[Int])
case_7a' list = (fs,ps)
                where posx = indexOf (end_of_block xs len) xs
                      ys = flip_stack posx xs
                      posy = ((indexOf ((end_of_block xs len) -1) ys) -1)
                      xs = fst list
                      x = head xs
                      path = snd list
                      fs = flip_stack posy ys
                      ps = path ++ [posx,posy]
                      len = length(fst(list))

case_7b :: ([Int],[Int]) -> ([Int],[Int])
case_7b list = (fs,ps)
                where posx = indexOf ((end_of_block xs len) +1) xs
                      ys = flip_stack posx xs
                      posy = ((indexOf (end_of_block xs len) ys) -1)
                      xs = fst list
                      x = head xs
                      path = snd list
                      fs = flip_stack posy ys
                      ps = path ++ [posx,posy]
                      len = length(fst(list))

case_7b' :: ([Int],[Int]) -> ([Int],[Int])
case_7b' list = (fs,ps)
                where posx = indexOf ((end_of_block xs len) -1) xs
                      ys = flip_stack posx xs
                      posy = ((indexOf (end_of_block xs len) ys) -1)
                      xs = fst list
                      x = head xs
                      path = snd list
                      fs = flip_stack posy ys
                      ps = path ++ [posx,posy]
                      len = length(fst(list))


-- One case should fit each time
solve :: ([Int],[Int]) -> ([Int],[Int])
solve t  | xs == goal = t
         | xs == reverse(goal) = (reverse(xs),(p ++ [(length xs)]))
         | free x xs && free (x+1) xs = solve (case_1a t)
         | free x xs && free (x-1) xs = solve (case_1b t)
         | free x xs && first_of_block (x+1) xs = solve (case_2a t)
         | free x xs && first_of_block (x-1) xs = solve (case_2b t)
         | free x xs && last_of_block (x+1) xs = solve (case_3b t)
         | free x xs && last_of_block (x-1) xs = solve (case_3c t)
         | in_a_block x xs && free (x+1) xs = solve (case_4a t)
         | in_a_block x xs && free (x-1) xs = solve (case_4b t)
         | in_a_block x xs && first_of_block (x+1) xs = solve (case_5a t)
         | in_a_block x xs && first_of_block (x-1) xs = solve (case_5b t)
         | in_a_block x xs && in_a_block (x+1) xs && diff_blocks x (x+1) xs && free prev xs && (posp < posy) = solve (case_6a' t)
         | in_a_block x xs && in_a_block (x-1) xs && diff_blocks x (x-1) xs && free next xs && (posn < posy) = solve (case_6a t)
         | in_a_block x xs && in_a_block (x+1) xs && diff_blocks x (x+1) xs && free prev xs && (posp > posy) = solve (case_6b' t)
         | in_a_block x xs && in_a_block (x-1) xs && diff_blocks x (x-1) xs && free next xs && (posn > posy) = solve (case_6b t)
         | in_a_block x xs && in_a_block (x+1) xs && diff_blocks x (x+1) xs && last_of_block (x+1) xs && in_a_block prev xs && first_of_block prev xs = solve (case_7a' t)
         | in_a_block x xs && in_a_block (x-1) xs && diff_blocks x (x-1) xs && last_of_block (x-1) xs && in_a_block next xs && first_of_block next xs = solve (case_7a t)
         | in_a_block x xs && in_a_block (x+1) xs && diff_blocks x (x+1) xs && last_of_block (x+1) xs && in_a_block prev xs && last_of_block prev xs = solve (case_7b' t)
         | in_a_block x xs && in_a_block (x-1) xs && diff_blocks x (x-1) xs && last_of_block (x-1) xs && in_a_block next xs && last_of_block next xs = solve (case_7b t)
         | otherwise = solve (reverse(fst t),((snd t) ++ [(length xs)]))
         where xs = fst t
               x = head xs
               p = snd t
               goal = quicksort xs
               next = end_of_block xs len +1
               prev = end_of_block xs len -1
               posx = indexOf (x-1) xs
               posy = indexOf (x+1) xs
               posn = indexOf next xs
               posp = indexOf prev xs
               len = length(fst(t))

gates :: [Int] -> [Int]
gates xs = snd (solve ((lengthish xs),[]))
