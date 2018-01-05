let rec zipperN (a: int list list) =
	match a with
	(hh :: []) :: [] -> hh :: []
	|(hh :: []) :: tl -> hh :: (zipperN (tl))
	|(hh :: ht) :: tl -> hh :: (zipperN (tl @ [ht]))
	| _ -> []
