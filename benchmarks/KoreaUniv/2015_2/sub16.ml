(*********************)
(* Problem 1: filter *)
(*********************)
let rec filter pred lst = 
let answer = [] in
match lst with
[] -> answer
| hd::tl -> if (pred hd) then hd::(filter pred tl)@answer else filter pred tl
	
(*********************)
(* Problem 2: zipper *)
(*********************)
let rec zipper : int list * int list -> int list
=fun (a,b) -> 
let result = [] in 
match a with 
[] -> if b = [] then result else result@b
| hd::tl -> (match b with
[] -> if tl =[] then result@[hd] else result@[hd]@(zipper (tl, []))  
| hd_b::tl_b -> hd :: hd_b :: (zipper (tl, tl_b)) @ result)

(*******************)
(* Problem 3: iter *)
(*******************)
let rec iter : int * (int -> int) -> (int -> int)
=fun (n, f) -> 
	match n with
		0 -> (fun x -> x)
		|1 -> f
		|_ -> fun x -> (iter(n-1, f) (f x)) (*(fun x -> (iter(n-1, f) x))*)

(*********************)
(* Problem 4: Diff   *)
(*********************)
type aexp = 
  | Const of int 
  | Var of string 
  | Power of string * int 
  | Times of aexp list
  | Sum of aexp list

let rec exist: aexp list*string -> int
= fun (l, x) ->
match l with 
[] -> 0
|hd::tl-> match hd with 
	Var v -> if v = x then 1 + (exist (tl, x)) else exist (tl, x)
	|Power (str, i) -> if str = x then 1+(exist (tl, x)) else exist (tl, x)
	|Times lst -> exist (lst, x) + exist (tl, x)
	|Sum lst -> exist (lst, x) + exist (tl, x)
	|_ -> 0 + exist (tl, x) 

let rec modify: aexp list*string -> aexp list
= fun (l, x) ->
match l with 
[] -> []
|hd::tl -> (match hd with
	Const n -> Times[Power(x, 0); Const n]::(modify (tl, x))
	|_ -> hd::(modify (tl, x)))

let normalized: aexp*string -> aexp
= fun (l, x) ->
let modified = modify([l], x) in 
match modified with 
[]->raise (Failure "error")
|hd::tl -> hd

let rec diff: aexp * string -> aexp
= fun (aexp, x) ->
if exist ([aexp], x) > 0 then
(match aexp with
Const n -> Const 0
|Var str -> Const 1
|Power (str, n) -> Times[Const n; Power(str, (n-1))]
|Times l -> (match l with
	[] -> Const 1
	|hd::tl -> (match hd with
		 Const n -> Times[Const n; diff(Times tl, x)]
		|Power(str1, n1) -> if n1=0 then Times[Const 0; diff(Times tl, x)]
								else Times[diff(hd, x); diff(Times tl,x)]
		|_ -> Times[diff(hd, x); diff(Times tl,x)])
	)
|Sum l -> let mod_exp = normalized(aexp, x) in (match mod_exp with 
	Sum l1 -> (match l1 with 
		[] -> Const 0
		|hd::tl -> Sum[diff(hd, x); diff(Sum tl, x)]
	  )
	|_ -> raise (Failure "error")
	)
)else (match aexp with
	Times [] -> Const 1
	|_ -> Const 0)

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
 
let rec replace_X : exp * int -> exp
= fun (e, x) ->
match e with 
X -> INT x
|INT n -> INT n
|SIGMA (a, b, c) -> raise (Failure "why here? error")
|ADD(a, b) -> ADD((replace_X(a, x)), (replace_X(b, x)))
|SUB(a, b) -> SUB((replace_X(a, x)), (replace_X(b, x)))
|MUL(a, b) -> MUL((replace_X(a, x)), (replace_X(b, x)))
|DIV(a, b) -> DIV((replace_X(a, x)), (replace_X(b, x)))

let rec num_calculate : exp -> int
= fun e ->
match e with
X -> raise (Failure "error")
|INT n -> n
|ADD (a, b) -> (num_calculate a) + (num_calculate b)
|SUB (a, b) -> (num_calculate a) - (num_calculate b)
|MUL (a, b) -> (num_calculate a) * (num_calculate b)
|DIV (a, b) -> (num_calculate a) / (num_calculate b)
|SIGMA (a, b, c) -> let start = (num_calculate a) in
		let endsig = (num_calculate b) in
			if start < (endsig + 1) then
				let func = replace_X (c, start) in
 				(num_calculate func) + num_calculate(SIGMA(ADD(INT start, INT 1), INT endsig, c))
			else 0

let calculator : exp -> int
= fun e ->
match e with 
X -> raise (Failure "error")
|INT n -> n
|_ -> num_calculate e
