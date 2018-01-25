(*********************)
(* Problem 1: filter *)
(*********************)
let rec filter pred lst = match lst with
			| [] -> []
			| hd::tl -> if pred hd = true then hd::(filter pred tl)
						else filter pred tl

 (* TODO *)

(*********************)
(* Problem 2: zipper *)
(*********************)
let rec zipper ((a : int list),(b : int list)) = match a with
			| [] -> if b = [] then []
					else zipper(b,a)
			| hd::tl -> hd::(zipper(b,tl))
			
 (* TODO *)

(*******************)
(* Problem 3: iter *)
(*******************)

let rec iter (n,(func : int -> int)) = if n = 0 then func
					else iter((n-1),func)

 (* TODO *)

(*********************)
(* Problem 4: Diff   *)
(*********************)
type aexp = 
  | Const of int 
  | Var of string 
  | Power of string * int 
  | Times of aexp list
  | Sum of aexp list

let rec diff : aexp * string -> aexp = fun (aexp, dif_var) ->
		match aexp with
		| Const n -> Const 0
		| Var var -> if(var=dif_var) then Const 1 else Const 0
		| Power (var, n) -> if (var=dif_var) then (if n>1 then Times[Const n; Power(var,(n-1))] else if (n=1) then Const 1 else Const 0) else Const 0
		| Times aexp_list -> (match aexp_list with
					| [] -> Const 0
					| hd::tl -> if tl = [] then diff(hd,dif_var) else Sum[Times(diff(hd,dif_var)::tl);Times[hd;diff(Times tl,dif_var)]])
		| Sum aexp_list -> (match aexp_list with
				|[] -> Const 0
				|hd::tl -> if tl=[] then diff(hd,dif_var) else Sum [diff(hd,dif_var);diff(Sum tl, dif_var)])
(*************************)
(* Problem 5: Calculator *)
(*************************)
type exp = X
         | INT of int
         | ADD of exp * exp
         | SUB of exp * exp
         | MUL of exp * exp
         | DIV of exp * exp
         | SIGMA of exp * exp * exp
let calculator : exp -> int
=fun e -> 0
