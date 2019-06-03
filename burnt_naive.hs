import Functions

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
