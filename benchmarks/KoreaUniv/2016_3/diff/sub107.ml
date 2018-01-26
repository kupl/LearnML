exception NotImplemented

(* Problem 1 *)

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
	| Const n -> Const 0
  	| Var x -> if x = var then Const 1 else Const 0
	| Power(s, n) ->
		if s = var then
		(match n with
		|0 -> Const 0
		|1 -> Const 1
		|2 -> Times [Const 2 ; Var var]
		|_ -> Times [Const n; Power(s, n-1)])
		else Const 0
	| Sum l ->
	(match l with
		[] -> Const 0
		| hd::[] -> (diff(hd,var))
		| hd::tl -> (Sum [diff(hd,var) ; diff(Sum tl, var)]))
	|Times l ->
	(match l with
		[] -> Const 1
		| hd :: [] -> (diff(hd, var))
		| hd :: tl ->
			(match hd with
				Const 1 -> (diff(Times tl, var))
				| Const n -> (Times[Const n ; diff(Times tl, var)])
				| _ -> (Sum [Times (diff(hd, var)::tl) ; Times [hd ; diff(Times tl, var)]]))) 

end

(* Problem 2 *)

module Problem2 = struct

type mobile = branch * branch
and branch =
| SimpleBranch of length * weight
| CompoundBranch of length * mobile
and length = int
and weight = int

let rec balanced : mobile -> bool
	= fun mob ->
	match mob with
	| SimpleBranch (lblen, lbwei), SimpleBranch (rblen, rbwei) 
		-> if lblen*lbwei = rblen*rbwei then true	else false
	| CompoundBranch (cblen, cmob), SimpleBranch (rblen, rbwei) 
		-> balanced cmob
	| SimpleBranch (lblen, lbwei), CompoundBranch (cblen, cmob) 
		-> balanced cmob
	| CompoundBranch (cblen1, cmob1), CompoundBranch(cblen2, cmob2)
  	-> balanced cmob1 && balanced cmob2
end


(* Problem 3 *)

module Problem3 = struct

type exp =
| X
| INT of int
| ADD of exp * exp
| SUB of exp * exp
| MUL of exp * exp
| DIV of exp * exp
| SIGMA of exp * exp * exp

let rec exptofun : exp -> (int -> int)
= fun e ->
	match e with
	| X -> (fun x -> x)
	| INT n -> (fun x -> x) 
	| ADD (e1,e2) -> (fun x -> (((exptofun e1) x) + ((exptofun e2) x)))
	| SUB (e1,e2) -> (fun x -> (((exptofun e1) x) - ((exptofun e2) x)))
	| MUL (e1,e2) -> (fun x -> (((exptofun e1) x) * ((exptofun e2) x)))
	| DIV (e1,e2) -> (fun x -> (((exptofun e1) x) / ((exptofun e2) x)))
	| _ -> (fun x -> 0)

let rec sigma : int * int * (int -> int) -> int
= fun (a,b,f) ->
	if a=b then f a
		else (f a) + (sigma (a+1,b,f));;

let rec reculse : exp -> int
= fun e ->
match e with
	| X -> raise (Failure "Error")
	| INT n -> n
	| ADD (INT n1,INT n2) -> n1+n2
	| ADD (e1, e2) -> reculse e1 + reculse e2
	| SUB (INT n1,INT n2) -> n1-n2
	| SUB (e1, e2) -> reculse e1 - reculse e2
	| MUL (INT n1,INT n2) -> n1*n2
	| MUL (e1, e2) -> reculse e1 * reculse e2
	| DIV (INT n1,INT n2) -> n1/n2
	| DIV (e1, e2) -> reculse e1 / reculse e2
	| SIGMA (INT a, INT b, e) -> sigma (a, b, (exptofun e))
	| SIGMA (e1, e2, e3) -> sigma (reculse e1, reculse e2, (exptofun e3))

let rec calculator : exp -> int
= fun exp ->
	(match exp with
	X -> 0
| INT n -> n
| ADD (e1,e2) -> (reculse e1 + reculse e2)
| SUB (e1,e2) -> (reculse e1 - reculse e2)
| MUL (e1,e2) -> (reculse e1 * reculse e2)
| DIV (e1,e2) -> (reculse e1 / reculse e2)
| SIGMA (e1,e2,e3) -> sigma (reculse e1, reculse e2, exptofun e3))

end


(* Problem4 *)

module Problem4 = struct
	type exp =
	| V of var
	| P of var * exp
	| C of exp * exp
	and var = string

let rec check2 : exp*(var list) -> bool
= fun (e,l) ->
	match e with
	|V v ->
		(match l with
		|[] -> false
		|hd::tl -> if v = hd then true else check2(V v, tl))
	|P(v, e1) -> check2(e1, v::l)
	|C(e1, e2)-> if check2(e1,l) = true && check2(e2,l) = true then true
		else false

let rec check : exp -> bool
= fun exp -> check2(exp, [])

end