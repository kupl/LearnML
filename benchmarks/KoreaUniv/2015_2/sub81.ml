(*********************)
(* Problem 1: filter *)
(*********************)
let rec filter pred lst = match lst with
|hd::tl-> if pred hd then hd::(filter pred tl) else filter pred tl
|[]->[]

(*********************)
(* Problem 2: zipper *)
(*********************)
let rec zipper : int list * int list -> int list
=fun (a,b) -> match b with
|[]->a@b
|hb::tb->
(match a with
|[]->b
|ha::ta->ha::hb::(zipper (ta,tb)))				

(*******************)
(* Problem 3: iter *)
(*******************)

let rec iter : int * (int -> int) -> (int -> int)
=fun (n,f) -> if n <=0
then fun x -> x
else fun x -> f (iter (n-1,f) x)

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
=fun (aexp,x) -> match aexp with
| Var x -> Const 1
| Power (x, n) -> Times [Const n; Power (x, n-1)]
| Times [a; b] -> Times [a; (diff (b, x))]
| Sum l -> (match l with 
	|hd::tl -> Sum ((diff (hd,x))::[diff(Sum tl, x)])
	|[] -> Const 0)
| _ -> Const 0

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

type opt = O of exp
				 | N

let rec eval : exp -> opt -> int
= fun e o-> match e with
| X -> (match o with
		|O x -> eval x N 
		|N -> raise (Failure "Cannot Identify"))
| INT n -> n
| ADD (e1,e2) -> (eval e1 o) + (eval e2 o)
| SUB (e1,e2) -> (eval e1 o) - (eval e2 o)
| MUL (e1,e2) -> (eval e1 o) * (eval e2 o)
| DIV (e1,e2) -> (eval e1 o) / (eval e2 o)
| SIGMA (e1, e2, e3) ->  if (eval e1 o) <= (eval e2 o)
	then
		let o = O e1
		in (eval e3 o) + (eval (SIGMA (ADD (e1, INT 1), e2, e3)) o)
	else 0

let calculator : exp -> int
=fun e -> eval e N
