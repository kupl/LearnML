(*********************)
(* Problem 1: filter *)
(*********************)
let rec filter pred lst = [] (* TODO *)

let rec filter pred lst =
 match lst with
[] -> []
| hd::tl -> if pred hd then hd :: (filter pred tl) else filter pred tl;;

(*********************)
(* Problem 2: zipper *)
(*********************)
let rec zipper : int list * int list -> int list
=fun (a,b) -> [] (* TODO *)

let rec zipper a b = 
 match (a,b) with
([],[]) -> []
| (hd::tl,[]) -> hd::tl
| ([],hd::tl) -> hd::tl
| (hd1::tl1,hd2::tl2) -> hd1 :: (zipper (hd2::tl2) tl1);;

(*******************)
(* Problem 3: iter *)
(*******************)

let rec iter : int * (int -> int) -> (int -> int)
=fun (n,f) -> f (* TODO *)

let rec func (n,f,a) = 
 match n with
0 -> a
| 1 -> f a
| x -> func(x-1,f,(f a));;

let rec iter (n,f) = fun a -> func(n,f,a);;

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
=fun (aexp,x) -> aexp (* TODO *)

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
=fun e -> 0 (* TODO *)

let rec calculator input =
 match input with
INT i -> i
| ADD (a,b) -> calculator(a) + calculator(b)
| SUB (a,b) -> calculator(a) - calculator(b)
| MUL (a,b) -> calculator(a) * calculator(b)
| DIV (a,b) -> calculator(a) / calculator(b)
| SIGMA (a,b,c) -> if (calculator(a))<=(calculator(b))
                   then func1 (calculator(a)) c +
                        calculator(SIGMA(INT( calculator(ADD(a,INT(1))) ),  b,  c ))
                   else 0
(*func1 : int,exp -> int*)
and func1 a c =
 match c with
INT i -> i
| ADD(a1,a2) -> if a1=X && a2=X then a+a
              else if a1=X && a2<>X then a+func1 a a2
              else if a1<>X && a2=X then func1 a a1+a
              else func1 a a1+func1 a a2
| SUB(a1,a2) -> if a1=X && a2=X then 0
                else if a1=X && a2<>X then a-func1 a a2
                else if a1<>X && a2=X then func1 a a1-a
                else func1 a a1-func1 a a2
| MUL(a1,a2) -> if a1=X && a2=X then a*a
                else if a1=X && a2<>X then a*func1 a a2
                else if a1<>X && a2=X then func1 a a1*a
                else func1 a a1*func1 a a2
| DIV(a1,a2) -> if a1=X && a2=X then a/a
                else if a1=X && a2<>X then a/func1 a a2
                else if a1<>X && a2=X then func1 a a1/a
                else func1 a a1/func1 a a2;;

