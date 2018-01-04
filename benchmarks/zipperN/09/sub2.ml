let rec zipperN (l:int list list) = 
	match l with
	(head::[])::tail -> head::(zipperN(tail))
	|(head::heta)::tail -> head::(zipperN(tail@[heta]))
	| _ -> []
	
