
  type lambda =
  | V of var
  | P of var * lambda
  | C of lambda * lambda
	and var = string

	let rec save : lambda -> var list
	= fun lambda ->
	match lambda with
	V a -> []
 |P (a,b) -> a::[]
 |C (a,b) -> save(a) @ save(b)

	let rec search : lambda * var list -> bool
	= fun (lambda, all) ->
match lambda with
	V a ->begin  match all with [] -> false |  hd::tl  -> if hd = a then true else search (V a, tl) end
	|P(a,b) -> search (b, (a::all))
	|C(a,b) -> search(a,all) && search(b, all)

	let rec check : lambda -> bool
  = fun lambda -> 
	match lambda with
	V a -> false
	|P (a , b) ->  search(b, a::[])
	|C (a , b) -> check (a) && check (b)(* TODO *)
