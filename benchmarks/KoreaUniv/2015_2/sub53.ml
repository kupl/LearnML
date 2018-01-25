(* Problem 1: filter *) (*********************) let rec filter pred lst = []


let rec filter f l =
match l with
[] -> []
| h::t ->
if (f h) = true then h::(filter f t)
else filter f t;;



(* Problem 2: zipper *) let rec zipper : int list * int list -> int list =fun (a,b) -> [] 


let rec zipper (l1, l2) =
match l1 with
[] -> l2
|h::t -> h::zipper(l2,t);;



(* Problem 3: iter *)let rec iter : int * (int -> int) -> (int -> int) =fun (n,f) -> f 


let rec iter(n,f) a =
if n = 0 then a
else iter (n-1, f) (f a) ;;




(* Problem 4: Diff *) type aexp = | Const of int | Var of string | Power of string * int | Times of aexp list | Sum of aexp list let rec diff : aexp * string -> aexp =fun (aexp,x) -> aexp 


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
|hd::tl -> Sum [diff(hd,x); diff(Sum tl, x)])


(* Problem 5: Calculator *) type exp = X | INT of int | ADD of exp * exp | SUB of exp * exp | MUL of exp * exp | DIV of exp * exp | SIGMA of exp * exp * exp let calculator : exp -> int =fun e -> 0


type exp = X
|INT of int
|ADD of exp * exp
|SUB of exp * exp
|MUL of exp * exp
|DIV of exp * exp
|SIGMA of exp * exp * exp;;


let rec calculator : exp -> int
= fun f - >
match f with
|INT a -> a
|ADD (a,b) -> calculator a + calculator b
|SUB (a,b) -> calculator a - calculator b
|MUL (a,b) -> calculator a * calculator b
|DIV (a,b) -> calculator a / calculator b
|SIGMA(INT a,INT b,f) -> calculator a + SIGMA(a+1,b,f) ;;
