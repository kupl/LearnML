(*
  1. You can modify the given function specifications as recursive.
  2. Do not modify the function names or types.
  3. It is free to define any helper functions.
*)

(*********************)
(*     Problem 1     *)
(*********************)
module Problem1 = struct
	type aexp = 
	  | Const of int
	  | Var of string
	  | Power of string * int
	  | Times of aexp list
	  | Sum of aexp list
	;;

	let rec diff : aexp * string -> aexp
	  = fun (exp, var) -> 
	    match exp with
	    | Const(_) -> Const 0
	    | Var(x) ->
	      if x = var
	      then Const 1
	      else Var x
	    | Power(x, const) ->
	      if x = var
	      then Times [Const const; Power(x, const - 1)]
	      else Power(x, const)
	    | Times(list) ->
	      begin
	      match list with
	      | [] -> Const 1
	      | hd::tl -> Sum[Times(diff(hd, var) :: tl); diff(Times(tl), var)]
	      end
	    | Sum(list) ->
	      begin
	      match list with
	      | [] -> Const 0
	      | hd::tl -> Sum[diff(hd, var); diff(Sum(tl), var)]
	      end
	;;
end

(*********************)
(*     Problem 2     *)
(*********************)
module Problem2 = struct
  type mobile = branch * branch
	  and branch = 
	  | SimpleBranch of length * weight
	  | CompoundBranch of length * mobile
	  and length = int
	  and weight = int
	;;

	let rec branchWeight : branch -> int
	  = fun b ->
	    match b with
	    | SimpleBranch(l, w) -> w
	    | CompoundBranch(l, m) ->
	      begin
	        match m with
	        | (b1, b2) -> branchWeight(b1) + branchWeight(b2)
	      end
	;;

	let rec mobileWeight : mobile -> int
	  = fun mob ->
	    match mob with
	    | (b1, b2) -> branchWeight(b1) + branchWeight(b2)
	;;

	let rec balanced : mobile -> bool
	  = fun mob ->
	    match mob with
	    | SimpleBranch(l1, w1), SimpleBranch(l2, w2) ->
	      if l1 * w1 = l2 * w2
	      then true
	      else false
	    | SimpleBranch(l1, w1), CompoundBranch(l2, m) ->
	      begin
	        balanced(m) && (l1 * w1 = l2 * mobileWeight(m))
	      end
	    | CompoundBranch(l1, m), SimpleBranch(l2, w2) ->
	      begin
	        balanced(m) && (l2 * w2 = l1 * mobileWeight(m))
	      end
	    | CompoundBranch(l1, m1), CompoundBranch(l2, m2) ->
	      begin
	        balanced(m1) && balanced(m2) && (l1 * mobileWeight(m1) = l2 * mobileWeight(m2))
	      end
	;;
end

(*********************)
(*     Problem 3     *)
(*********************)
module Problem3 = struct
  type exp =
  | X
  | INT of int
  | ADD of exp * exp
  | SUB of exp * exp
  | MUL of exp * exp
  | DIV of exp * exp
  | SIGMA of exp * exp * exp
	;;

	let rec value: exp -> int
	  = fun expression ->
	    match expression with
	    | INT(n) -> n
	    | ADD(n1, n2) -> value(n1) + value(n2)
	    | SUB(n1, n2) -> value(n1) - value(n2)
	    | MUL(n1, n2) -> value(n1) * value(n2)
	    | DIV(n1, n2) -> value(n1) / value(n2)
	    | _ -> 0
	;;

	let rec evaluate : exp -> exp -> exp
	  = fun index expression ->
	    match expression with
	    | X -> index
	    | INT(_) -> expression
	    | ADD(n1, n2) -> ADD((evaluate index n1), (evaluate index n2))
	    | SUB(n1, n2) -> SUB((evaluate index n1), (evaluate index n2))
	    | MUL(n1, n2) -> MUL((evaluate index n1), (evaluate index n2))
	    | DIV(n1, n2) -> DIV((evaluate index n1), (evaluate index n2))
	    | SIGMA(n1, n2, f) -> 
	      if value (evaluate index n1) > value (evaluate index n2)
	      then INT(0)
	      else ADD((evaluate n1 f), (evaluate (ADD(n1, INT(1))) (SIGMA((ADD(n1, INT(1))), n2, f))))
	;;

	let calculator : exp -> int
	  = fun e ->
	    value (evaluate (INT(0)) e)
	;;
end

(*********************)
(*     Problem 4     *)
(*********************)
module Problem4 = struct
  type exp =
	| V of var
	| P of var * exp
	| C of exp * exp
	and var = string
	;;

	let rec has : string -> exp -> bool
	  = fun str expression ->
	    match expression with
	    | V(v) -> false
	    | P(v, e) ->
	      if v = str then true else false
	    | C(e1, e2) ->
	      (has str e1) || (has str e2)
	;;

	let rec checkOriginal : exp -> exp -> bool
	  = fun exp original ->
	    match exp with
	    | V(v) -> (has v original)
	    | P(v, e) -> (checkOriginal e original)
	    | C(e1, e2) -> (checkOriginal e1 original) && (checkOriginal e2 original)

	let rec check : exp -> bool
	  = fun exp ->
	    checkOriginal exp exp
	;;
end

