(*
  1. You can modify the given function specifications as recursive.
  2. Do not modify the function names or types.
  3. It is free to define any helper functions.
*)

exception NotImplemented

  type exp =
  | X
  | INT of int
  | ADD of exp * exp
  | SUB of exp * exp
  | MUL of exp * exp
  | DIV of exp * exp
  | SIGMA of exp * exp * exp

  let sum = ref 0;; 
  let refer = ref 0 ;;

  let renew k n =
		let save = k := n in
		    save ;;

  let sigma n =
		let sigm = sum := !sum + n in
				sigm ;;
  
  let rec calculator : exp -> int
  = fun exp -> match exp with
    X          -> !refer
	|	INT a      -> a
	| ADD (a, b) -> calculator a + calculator b
	| SUB (a, b) -> calculator a - calculator b
  | MUL (a, b) -> calculator a * calculator b 
	| DIV (a, b) -> if calculator b = 0 then raise NotImplemented
									else calculator a / calculator b
	| SIGMA(a, b, c) -> if calculator a > calculator b
										  then let temp = !sum in
														let () = renew sum 0 in temp
		                  else let () = renew refer (calculator b) in
														let () = sigma (calculator c) in
															calculator (SIGMA (a,INT(calculator b - 1),c));;