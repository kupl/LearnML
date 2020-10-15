let rec sigma f a b  =
	if a = b then f a
	else if a < b then f a + sigma f (a+1) b
  else (* a > b *) 0
