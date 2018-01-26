(*     Problem 1     *)
module Problem1 = struct
   type aexp =
   | Const of int
   | Var of string
   | Power of string * int
   | Times of aexp list
   | Sum of aexp list
 
   let rec diff : aexp * string -> aexp
   = fun (exp, var) ->
   match exp with
   |Const a -> Const a
   |Var x -> Var x (* if x=var then Const 1 else Var x*)
   |Power (x, n) ->
    if x=var then Times[Const n; Power(x, n-1)]
    else Power(x, n)
   |Times l -> begin 
    match l with
    |[] -> raise(Failure "error")
    |h::t -> Sum[Times(diff (h, var)::t) ; diff (h, var)]
    end
   |Sum m ->
    match m with
    |[] -> raise(Failure "error")
    |h::t -> Sum[diff (h, var); diff (h,var)]

end 

(*
(*     Problem 2     *)
module Problem2 = struct
  type mobile = branch * branch
  and branch = 
  | SimpleBranch of length * weight
  | CompoundBranch of length * mobile
  and length = int
  and weight = int

  let balanced : mobile -> bool
  = fun mob -> raise NotImplemented (* TODO *)
end
*)

(*     Problem 3     *)
module Problem3 = struct
   type exp =
   | X
   | INT of int
   | ADD of exp * exp
   | SUB of exp * exp
   | MUL of exp * exp
   | DIV of exp * exp
   | SIGMA of exp * exp * exp


let rec calculator :exp -> int
   = fun exp ->
	match exp with
	|INT n -> n
	|ADD (e1, e2) ->
		let n1 = calculator (e1) in
		let n2 = calculator (e2) in
			n1 + n2
	|SUB(e1, e2) -> 
		let n1 = calculator (e1) in
	  let n2 = calculator (e2) in
			n1 + n2
	|MUL(e1,e2) ->
		let n1 = calculator (e1) in
		let n2 = calculator (e2) in
			n1 * n2	
	|DIV(e1,e2) ->
		let n1 = calculator (e1) in
		let n2 = calculator (e2) in
			n1 mod n2
	(*|SIGMA(e1, e2, f) ->
		let rec sigma f a b = 
		let a = calculator (e1) in
		let b = calculator (e2) in 
			if a = b then (f a)
			else(f a) + sigma f (a+1) b*)
end

(*
(*********************)
(*     Problem 4     *)
(*********************)
module Problem4 = struct
  type exp =
  | V of var
  | P of var * exp
  | C of exp * exp
  and var = string

  let check : exp -> bool
  = fun exp ->
end
*)