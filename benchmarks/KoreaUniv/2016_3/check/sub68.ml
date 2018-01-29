
  type exp =
  | V of var
  | P of var * exp
  | C of exp * exp
  and var = string
  let rec g e s =
		match e with
		V a -> let rec f a b = match a with [] -> false | h::t -> if h = b then true else (f t b) in if (f s a) then true else false |
		P (v, e1) -> if (g e1 (s@[v])) then true else false |
		C (e1, e2) -> if (g e1 s && g e2 s) then true else false;;
  let check : exp -> bool
  = fun exp -> g exp [];;
