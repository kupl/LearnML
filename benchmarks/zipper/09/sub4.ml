let rec zipper a b =
	match a, b with
	| [], [] -> []
	| h1 :: t1, [] -> a
	| [], h2 :: t2 -> b
	| h1 :: t1, h2 :: t2 -> h1 :: h2 :: (zipper t1 t2)
;;