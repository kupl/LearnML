{
  P (("x"), C (V ("x"), P (("x"), V ("y")))) -> false;
  P (("x"), C (V ("x"), P (("y"), V ("x")))) -> true;
  P (("y"), P (("x"), V ("y"))) -> true;
  P (("y"), P (("x"), P (("y"), V ("x")))) -> true;
  P (("x"), C (P (("y"), V ("x")), P (("z"), V ("y")))) -> false;
}

type exp =
  | V of string
  | P of string * exp
  | C of exp * exp

let rec eval : exp -> string list -> bool
= fun exp lst ->
	match exp with
	| V (var) -> 
  	(
  		match lst with
  		|[] -> false
  		|hd::tl -> if var = hd then true else eval exp tl
  	)
  | P (var, exp2) -> eval exp2 (var::lst)
  | C (exp1, exp2) -> (eval exp1 lst)&&(eval exp2 lst)
;;

let check : exp -> bool
= fun exp -> 
	let state = [] in
	match exp with
	| V (var) -> eval exp state
	| P (var, exp) -> eval exp (var::state)
	| C (exp1, exp2) -> (eval exp1 state)&&(eval exp2 state)
;;