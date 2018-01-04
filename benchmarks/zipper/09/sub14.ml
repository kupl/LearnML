let rec zipper a b =
	if a=[] then b
	else if b=[] then a
	else (List.hd a) :: (List.hd b) :: zipper (List.tl a) (List.tl b)