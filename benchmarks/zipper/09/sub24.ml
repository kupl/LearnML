let rec zipper :int list*int list
=fun (a,b) ->
	match a with
		x::lista -> (match b with y::listb -> x::y::zipper(lista,listb) | _ -> a)
		| _ -> if b = [] then [] else b;;
