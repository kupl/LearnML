let rec zipper a b =
	if a=[] then b
	else if b=[] then a
	else (List.hd a) :: (List.hd b) :: zipper (List.tl a) (List.tl b)

(*let _ = List.iter (fun x -> print_int x; print_newline()) (zipper [1;2;3] [4;5;6;7;8])*)