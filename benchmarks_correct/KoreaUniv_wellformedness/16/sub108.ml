
  type lambda =
  | V of var
  | P of var * lambda
  | C of lambda * lambda
  and var = string

  let check : lambda -> bool
  = fun lambda -> let rec inList li str = match li with
								| [] -> false
								| hd::tl -> if hd = str then true else inList tl str
							in let rec chk l ex = match ex with
								| V x -> inList l x
								| P (x,y) -> chk (x::l) y
								| C (x,y) -> chk l x && chk l y
							in chk [] lambda
