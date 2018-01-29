
  type exp =
  | V of var
  | P of var * exp
  | C of exp * exp
  and var = string

	let rec isthere : string->exp->exp
	= fun key exp -> match exp with
		| V v1 -> if key=v1 then V "true" else V v1
		| P (v1,e1) -> P (v1, isthere key (isthere v1 e1))
		| C (e1,e2) -> C (isthere key e1, isthere key e2)

	let rec nothere : exp -> bool
	= fun exp -> match exp with
		| V v -> if v="true" then true else false
		| P (v,e) -> if (nothere e)=true then true else false
		| C (e1,e2) -> if ((nothere e1)=true && (nothere e2)=true) then true else false

  let check : exp -> bool
  = fun exp -> match exp with
		| V v -> false
		| P (v,e) -> nothere (isthere v e)
		| C (e1,e2) ->  false
