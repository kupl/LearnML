(*********************)
(* Problem 1: filter *)
(*********************)
let rec filter pred lst = 
	match lst with
	| [] -> raise(Failure "List is too short!")
	| [a] -> if pred a = true then [a] else []
	| hd::tl -> if pred hd = true then hd::(filter pred tl)
	else filter pred tl (* TODO *)

(*********************)
(* Problem 2: zipper *)
(*********************)
let rec zipper : int list * int list -> int list
=fun (a,b) -> 
	match a with
	| [] -> b
	| hd::tl ->
		match b with
			|[] -> a
			|h::t -> hd::h::zipper (tl,t);;
 (* TODO *)

(*******************)
(* Problem 3: iter *)
(*******************)

let rec iter : int * (int -> int) -> (int -> int)
=fun (n,f) ->
	if n=0 then fun x -> x
	else fun x -> f (iter((n-1),f)x)
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

let rec diff : aexp * string -> aexp
=fun (aexp,x) ->
	match aexp with
	| Const n -> Const 0
	| Var str -> if str = x then Const 1 else Const 0
	| Power (str, num) ->
		if str = x
		then
			if num = 1 then   Const num
			else if num = 0 then Const 0
			else Times[Const num;Power(str, num-1)]
		else Const 0
	| Sum ls -> match ls with
		|[a] -> diff(a,x)
		|hd::tl -> Sum [diff(hd,x);diff(Sum tl,x)]
	|Times ls ->if List.mem (Const 0) ls then Const 0
              else (match ls with
                  | [a] ->diff(a,x);
                  | hd::tl ->match hd with
                            | Const a -> Times[hd;diff(Times tl,x)]
                            | _ ->Sum [Times [diff(hd,x);Times tl]; Times[hd;diff(Times tl,x)]])
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

let rec calculator : exp -> int
=fun e ->
	match e with
	|INT n -> n
	|ADD (e1,e2) -> calculator e1 + calculator e2
	|SUB (e1,e2) -> calculator e1 - calculator e2
	|MUL (e1,e2) -> calculator e1 * calculator e2
	|DIV (e1,e2) -> calculator e1 / calculator e2
		

	 (* TODO *)