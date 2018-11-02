(* problem 3*)
type formula =
    True
  | False
  | Var of string
  | Neg of formula
  | And of formula * formula
  | Or of formula * formula
  | Imply of formula * formula
  | Iff of formula * formula

let rec getEnv : 'a list -> string -> bool
= fun env x -> match env with
			| [] -> true
			| (k, v)::tl -> if k=x then v else (getEnv tl x)

let rec calc : formula -> 'a list -> bool
= fun f e -> match f with
		| True -> true
		| False -> false
		| Var(x) -> (getEnv e x)
		| Neg(x) -> let res_x = (calc x e) in
			    if res_x then false else true
		| And(x, y) -> let res_x = (calc x e) in
			       let res_y = (calc y e) in
			       if res_x then if res_y then true else false else false
		| Or(x, y) -> let res_x = (calc x e) in
			      let res_y = (calc y e) in
			      if res_x then true else if res_y then true else false
		| Imply(x, y) -> let res_x = (calc x e) in
				 let res_y = (calc y e) in
					if res_x then if res_y then true else false else true
		| Iff(x, y) -> let res_x = (calc x e) in
			       let res_y = (calc y e) in
					if res_x=res_y then true else false

let rec process : formula -> string list -> 'a list -> bool
= fun f env nenv-> match env with
			| [] -> (calc f nenv)
			| hd::tl -> if (process f tl ((hd, true)::nenv)) then true
					else if (process f tl ((hd, false)::nenv)) then true
					     else false

let rec makeEnv : formula -> 'a list
= fun f -> match f with
		| True -> []
		| False -> []
		| Var(x) -> [(x,false)]
		| Neg(x) -> (makeEnv x)
		| And(x, y) -> (makeEnv x)@(makeEnv y)
		| Or(x, y) -> (makeEnv x)@(makeEnv y)
		| Imply(x, y) -> (makeEnv x)@(makeEnv y)
		| Iff(x, y) -> (makeEnv x)@(makeEnv y)

let rec makeSet : 'a list -> string list
= fun l -> match l with
		| [] -> []
		| (hd, _)::tl -> if (getEnv tl hd)=false then (makeSet tl) else hd::(makeSet tl)

let sat : formula -> bool
= fun f -> let env = (makeEnv f) in (process f (makeSet env) [])
