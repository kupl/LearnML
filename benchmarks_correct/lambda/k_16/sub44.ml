
  type lambda =
  | V of var
  | P of var * lambda
  | C of lambda * lambda
  and var = string

  let rec check : lambda -> bool
= fun ex -> match ex with
	| _ -> calc ex []
and calc : lambda -> var list -> bool
= fun ex lst -> match ex with
	| V a -> deletion lst a
	| P (a,b) -> calc b (lst@[a])
	| C (a,b) -> if calc a lst = true && calc b lst = true then true else false
and deletion : var list -> var -> bool
= fun lst va -> match lst with
	| [] -> false
	| hd::tl -> if va = hd then true else (deletion tl va)
