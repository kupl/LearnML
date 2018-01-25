(*********************)
(* Problem 1: filter *)
(*********************)
let rec filter pred lst = match lst with
[] -> []
| hd::tl -> if pred(hd) then hd::(filter pred tl) else filter pred tl;;

(*********************)
(* Problem 2: zipper *)
(*********************)
let rec zipper : int list * int list -> int list
=fun (a,b) -> match a with
[] -> b
| hd::tl -> (match b with
[] -> a
| h::t -> hd::h::(zipper (tl,t)));;

(*******************)
(* Problem 3: iter *)
(*******************)

let rec iter : int * (int -> int) -> (int -> int)
=fun (n,f) -> match n with
0 -> fun x -> x
|_ -> if n<0 then raise(Failure "error") else
fun x -> f(iter(n-1,f) x);;

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
| Const a -> Const 0
| Var b -> if b=x then Const 1 else Var b
| Power(b,a) -> if b=x then Times [Const a; Power (x,a-1)] else Power(b,a)
| Times l -> (match l with
[] -> Const 0
| hd::tl -> (match hd with
Var b -> if b=x then Times ((diff (hd,x))::tl)
	else Times (hd::[diff (Times tl,x)])
| Power (b,a) -> if b=x then Times ((diff (hd,x))::tl)
	else Times (hd::[diff (Times tl,x)])
|_ -> Times (hd::[diff (Times tl,x)])))
| Sum l -> (match l with
[] -> Const 0
| hd::tl -> Sum ((diff (hd,x))::[diff (Sum tl,x)]));;

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
=fun e -> match e with
X -> raise(Failure "error")
| INT x -> x
| ADD(x,y) -> (calculator x) + (calculator y)
| SUB(x,y) -> (calculator x) - (calculator y)
| MUL(x,y) -> (calculator x) * (calculator y)
| DIV(x,y) -> (calculator x) / (calculator y)
| SIGMA(x,y,z) -> (let rec cal = fun e -> match e with
X -> cal y
| INT a -> a
| ADD(a,b) -> (cal a) + (cal b)
| SUB(a,b) -> (cal a) - (cal b)
| MUL(a,b) -> (cal a) * (cal b)
| DIV(a,b) -> (cal a) / (cal b)
| SIGMA(x,y,z) -> raise(Failure "error") in
if (calculator x) < (calculator y)
then (cal z) + calculator(SIGMA(x,(INT((calculator y)-1)),z))
else cal z);;
