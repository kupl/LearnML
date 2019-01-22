
  type exp =
  | V of var
  | P of var * exp
  | C of exp * exp
  and var = string

  let check : exp -> bool
  = fun exp -> let rec inList li str = match li with
								| [] -> false
								| hd::tl -> if hd = str then true else inList tl str
							in let rec chk l ex = match ex with
								| V x -> inList l x
								| P (x,y) -> chk (x::l) y
								| C (x,y) -> chk l x && chk l y
							in chk [] exp
