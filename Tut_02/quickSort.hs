quicksort [] = []
quicksort (x:xs) = (quicksort lesser) ++ [p] ++ (quicksort greater)
	where
		lesser = filter (< p) xs
		greater = filter (>= p) xs