let rec zipper(a,b:int list*int list) =
	match a with
		x::lista -> (match b with y::listb -> x::y::zipper(lista,listb) | _ -> a)
		| _ -> if b = [] then [] else b;;