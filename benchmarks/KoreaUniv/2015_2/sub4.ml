(*********************)
(* Problem 1: filter *)
(*********************)
let rec filter pred lst = 
match lst with
[] -> [] 
| h::t -> if pred h then h::filter pred t else filter pred t

(*********************)
(* Problem 2: zipper *)
(*********************)
let rec zipper : int list * int list -> int list
=fun (a,b)-> 
match a with
[]->b
|h1::t1->
match b with
[]-> a
|h2::t2->h1::h2::zipper (t1,t2)

(*******************)
(* Problem 3: iter *)
(*******************)

let rec iter : int * (int -> int) -> (int -> int)
=fun (n,f) a ->
if n=0 then a
else if n > 0 then f (iter(n-1,f) a)
else raise (Failure "n should be >=0")

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
|Const a -> Const 0
|Var "x" -> Const 1
|Power ("a", n) -> if n = 1 then Const 1 else Times [Const n; Power ("a", n-1)]
|Times [a;b] -> Times [diff (aexp,x); diff(aexp,x)]
|Sum [a;b] -> Sum [diff (aexp,x); diff(aexp,x)]

(*************************)
(* Problem 5: Calculator *)
(*************************)