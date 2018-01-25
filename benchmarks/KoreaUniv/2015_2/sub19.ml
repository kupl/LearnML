(*********************)
(* Problem 1: filter *)
(*********************)
let rec filter pred lst = match lst with
|[] -> []
|[x] -> if (pred x = true) then [x] else []
|h::t -> (filter pred [h]) @ (filter pred t)

(*********************)
(* Problem 2: zipper *)
(*********************)
let rec zipper : int list * int list -> int list
=fun (a,b) -> match a, b with
|[],_ -> b
|_,[] -> a
|h1::t1, h2::t2 -> [h1] @ [h2] @ zipper (t1, t2)

let compose f g = fun x -> f(g x)

(*******************)
(* Problem 3: iter *)
(*******************)

let rec iter : int * (int -> int) -> (int -> int)
=fun (n,f) -> if n = 0 then fun x -> x
else compose f (iter (n-1, f))

exception INVALID_INPUT

(*********************)
(* Problem 4: Diff   *)
(*********************)
type aexp = 
  | Const of int 
  | Var of string 
  | Power of string * int 
  | Times of aexp list
  | Sum of aexp list

let rec map f l x=
match l with
| [] -> []
| hd::tl -> (f (hd, x))::(map f tl x)

let rec diff : aexp * string -> aexp
=fun (aexp,x) -> match aexp with
Const n -> Const 0
|Var s -> if (s = x) then Const 1 else Const 0
|Power (s, n) -> if (s = x) then Times ([Const n] @ [Power (s, n-1)]) else Const 0
|Times [] -> Times []
|Times (hd::tl::[]) -> Sum ([Times ([diff (hd, x)] @ [tl])] @ [Times ([hd] @ [diff (tl, x)])])
|Times (hd::tl) -> Sum ([Times ([diff (hd, x)] @ tl)] @ [Times ([hd] @ [diff (Times tl, x)])])
|Sum lst -> Sum (map diff lst x)


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

let inc : exp -> exp
= fun e -> match e with
INT x -> INT (x+1)
|_ -> raise INVALID_INPUT

let rec str_replace : exp -> exp -> exp
= fun subject replace -> match subject with
X -> replace
| INT n -> INT n
| ADD (e1, e2) -> ADD (str_replace e1 replace, str_replace e2 replace)
| SUB (e1, e2) -> SUB ((str_replace e1 replace), (str_replace e2 replace))
| MUL (e1, e2) -> MUL ((str_replace e1 replace), (str_replace e2 replace))
| DIV (e1, e2) -> DIV ((str_replace e1 replace), (str_replace e2 replace))
| SIGMA (s, e, f) -> SIGMA ((str_replace s replace), (str_replace e replace), (str_replace f replace))

(*let rec calculator : exp -> int*)
let rec calculator : exp -> int
=fun e -> match e with
|X -> raise INVALID_INPUT
|INT n -> n
|ADD (e1, e2) -> calculator e1 + calculator e2
|SUB (e1, e2) -> calculator e1 - calculator e2
|MUL (e1, e2) -> calculator e1 * calculator e2
|DIV (e1, e2) -> calculator e1 / calculator e2
|SIGMA (s, e, f) -> if (s = e) then calculator (str_replace f s) else calculator (str_replace f s) + calculator (SIGMA (inc s, e, f))