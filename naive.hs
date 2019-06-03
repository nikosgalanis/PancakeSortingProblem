import Functions

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
