
  type exp =
  | V of var
  | P of var * exp
  | C of exp * exp
  and var = string

  let rec check : exp -> bool
= fun ex -> match ex with
	| _ -> calc ex []
and calc : exp -> var list -> bool
= fun ex lst -> match ex with
	| V a -> deletion lst a
	| P (a,b) -> calc b (lst@[a])
	| C (a,b) -> if calc a lst = true && calc b lst = true then true else false
and deletion : var list -> var -> bool
= fun lst va -> match lst with
	| [] -> false
	| hd::tl -> if va = hd then true else (deletion tl va)
