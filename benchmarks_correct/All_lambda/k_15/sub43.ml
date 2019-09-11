  type lambda = V of var
           | P of var * lambda
           | C of lambda * lambda
  and var = string
 
	let rec checklist : string * string list -> bool
	=fun (s,l) -> match l with
								| hd::tl -> if hd = s then true
														else checklist(s,tl)
								| [] -> false

	let rec parse : lambda * string list -> bool
	=fun (e,l) -> match e with
								| V a -> checklist(a,l)
								| P (v,e) -> parse(e,(v::l))
								| C (e1,e2) -> parse(e1,l) && parse(e2,l)
	
  let check : lambda -> bool
  =fun e -> parse (e,[])
