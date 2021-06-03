let rec max l = 
	match l with 
	| [] -> raise(Failure "list is too short..")
	| hd :: tl ->if tl = [] then hd else
				 if hd > max tl then hd 
				else max tl
 