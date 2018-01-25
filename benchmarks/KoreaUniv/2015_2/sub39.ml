(*********************)
(* Problem 1: filter *)
(*********************)
let rec filter : ('a -> bool) -> 'a list -> 'a list
= fun pred lst ->
match lst with
|[] -> []
|hd::tl->
if pred hd then hd::(filter pred tl)
else filter pred tl;;

(*********************)
(* Problem 2: zipper *)
(*********************)
let rec zipper : int list * int list -> int list
=fun (a,b) ->
match a, b with
|(_,[]) -> a
|([],_) -> b
|(ah::at, [y]) -> ah::y::at
|(ah::at, bh::bt)-> ah::bh::zipper(at, bt);; 

(*******************)
(* Problem 3: iter *)
(*******************)

let rec iter : int * (int -> int) -> (int -> int)
=fun (n,f) num-> 
if n = 0 then num
else f (iter (n-1,f) num);;

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
= fun (aexp,x) -> 
match aexp with
|Const int -> Const 0
|Var v-> 
if x = v then Const 1
 else Const 0
|Power (v, aexp') ->
 if x=v then Times [Const aexp'; Power (v, aexp'-1)]
 else Const 0
|Times lst ->
(match lst with
|[] -> Const 0
|hd::tl -> Sum [Times (diff (hd, x)::tl); Times [hd; diff (Times tl, x)]])
|Sum list ->
(match list with
|[] -> Const 0
|hd::tl -> Sum [diff(hd,x); diff(Sum tl, x)]);;

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
=fun exp -> 
let rec setFlag exp flag num result =
(match exp with
|X -> 
if flag = 0 then raise (Failure "FreeVariable")
else num
|INT a -> a
|ADD (ex1,ex2) -> (setFlag ex1 flag num result) + (setFlag ex2 flag num result)
|SUB (ex1,ex2) -> (setFlag ex1 flag num result) - (setFlag ex2 flag num result)
|MUL (ex1,ex2) -> (setFlag ex1 flag num result) * (setFlag ex2 flag num result)
|DIV (ex1,ex2) -> (setFlag ex1 flag num result) / (setFlag ex2 flag num result)
|SIGMA (ex1,ex2,ex3) ->
let ex1' = setFlag ex1 flag num result in
let ex2' = setFlag ex2 flag num result in 
let num = ex1' in 
let flag = 1 in 
(if ex1' > ex2' then result
else 
let result = result + (setFlag ex3 flag num result) in
setFlag (SIGMA (INT (ex1'+1), INT ex2', ex3)) flag num result)) 
in setFlag exp 0 0 0;;