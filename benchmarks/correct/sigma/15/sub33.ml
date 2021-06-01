let rec sigma f a b  =
	  if a > b
	  then 0

	  else if a = b
	  then f a
	  else let aa = a+1 in f a + sigma f aa b
