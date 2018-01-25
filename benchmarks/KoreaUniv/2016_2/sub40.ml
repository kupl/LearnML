Problem 1
let rec max : int list -> int
= fun lst -> match lst with | []->0 |  hd::tl ->
let compare a b = if a>b then a else b in
let rec fold compare m n =
match m with | []->n | hd::tl -> compare hd (fold compare tl n) in
fold compare tl hd;;

let rec min : int list -> int
= fun lst -> match lst with | []->0 | hd::tl ->
let compare a b = if a>b then b else a in
let rec fold compare m n =
match m with | []->n | hd::tl -> compare hd (fold compare tl n) in
fold compare tl hd;;

Problem 2
let rec filter pred lst = 
match lst with | []->[] | hd::tl ->
(if pred hd == true then [hd] else [])@(filter pred tl);;

Problem 3
let rec double f a = f(f(a));;

Problem 4
type btree = |Empty|Node of int*btree*btree
let rec mem : int -> btree -> bool = fun n tree ->
match tree with | Empty -> false | Node(a,b,c)
-> if a==n then true else mem n b || mem n c;;

Problem 5
type nat = ZERO | SUCC of nat

let rec natadd : nat->nat->nat = fun n1 n2 -> match n1 with |
ZERO -> n2 | SUCC(n3) -> SUCC(natadd n3 n2);;

let rec natmul : nat->nat->nat = fun n1 n2 ->
let rec mul a b c = match a with | ZERO -> c |
SUCC(a1) -> mul a1 b (natadd b c)in
mul n1 n2 ZERO;;

Problem 6
type formula = | True | False | Not of formula
| AndAlso of formula * formula
| OrElse of formula * formula
| Imply of formula * formula
| Equal of exp * exp

and exp =
| Num of int
| Plus of exp * exp
| Minus of exp * exp

let rec eval : formula -> bool = fun f -> match f with
| True -> true
| False -> false
| Not (a) -> if eval (a) = true then false  else true
| AndAlso (a,b) -> if eval(a) = true&&eval(b) = true then true else false
| OrElse (a.b) -> if eval(a) = false&&eval(b) = false then false else true
| Imply (a,b) -> if eval(a) = true&&eval(b)=false then false  else true
| Equal (a,b) -> let rec eq: exp -> int = fun e ->
  match e with | Num(a) -> a
  |Plus(a,b) -> eq(a) +eq(b)
  |Minus(a,b) -> eq(a)-eq(b) in
if eq(a)=eq(b) then true else false;;
