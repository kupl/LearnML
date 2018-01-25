(*********************)
(* Problem 1: filter *)
(*********************)
let rec filter pred lst =
  match lst with
  [] -> []
  |h::t -> if pred h then h::filter pred t else filter pred t

(*********************)
(* Problem 2: zipper *)
(*********************)
let rec zipper : int list * int list -> int list  =
	fun (a, b) -> match (a, b) with
	([], []) -> []	
	|(a, []) -> a
	|([], b) -> b
	|(h1::t1, h2::t2) -> if h1 > h2 then h2::zipper (a, t2) else h1::zipper (t1, b)

(*******************)
(* Problem 3: iter *)
(*******************)
let rec iter : int * (int -> int) -> (int -> int)
= fun (n, f) -> match n with
0 -> (fun x -> x)
|_ -> fun x -> (iter(n-1, f) (f x))

(*********************)
(* Problem 4: Diff   *)
(*********************)
type aexp = 
|Const of int
|Var of string
|Power of string * int
|Times of aexp list
|Sum of aexp list

let rec diff : aexp * string -> aexp
= fun(aexp, x) -> match aexp with
Const _ -> Const 0
|Var x -> Const 1
|Power (x, a) -> 
	(match a with
	0 -> Const 0
	|1 -> Const 1
	|2 -> Times [Const 2;Var x]
	|_ -> Times [Const a;Power (x, a-1)])
|Times l ->
	(match l with
	[] -> raise(Failure"NO INPUT")
	|[b] -> diff(b, x)
	|h::t -> Sum [Times (diff(h, x)::t) ;Times (h::[diff(Times t, x)])])
|Sum s ->
	(match s with
	[] -> raise(Failure"NO INPUT")
	|[b] -> diff(b, x)
	|h::t -> Sum [diff(h, x);diff(Sum t, x)])

(*************************)
(* Problem 5: Calculator *)
(*************************)
type exp = X
| INT of int
| ADD of exp * exp
|	SUB of exp * exp
| MUL of exp * exp
| DIV of exp * exp
| SIGMA of exp * exp * exp

let toexp : int -> exp
= fun x -> INT x

let rec toint : (int * exp) -> int
= fun(n, e) -> match (n, e) with
(0, X) -> raise(Failure"Error")
|(_, X) -> n
|(_, INT a) -> a
|(_, ADD (a, b)) -> (toint (n, a)) + (toint (n, b))
|(_, SUB (a, b)) -> (toint (n, a)) - (toint (n, b))
|(_, MUL (a, b)) -> (toint (n, a)) * (toint (n, b))
|(_, DIV (a, b)) -> (toint (n, a)) / (toint (n, b))
|(_, SIGMA(a, b, m)) -> if toint(0, a) < toint(0, b)
	then toint(toint(0, a), m) + toint(0, SIGMA(toexp(toint(0, a) +1), b, m))
	else toint(toint(0, b), m)

let calculator : exp -> int
= fun e -> match e with
X -> raise(Failure"Error")
|INT a -> a
|ADD (n, m) -> (toint (0, n)) + (toint (0, m))
|SUB (n, m) -> (toint (0, n)) - (toint (0, m))
|MUL (n, m) -> (toint (0, n)) * (toint (0, m))
|DIV (n, m) -> (toint (0, n)) / (toint (0, m))
|SIGMA (a, b, n) -> toint(0, SIGMA(a, b, n))
