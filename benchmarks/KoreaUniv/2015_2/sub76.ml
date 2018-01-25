(* 2011210039 Kang Seungwoo *)

exception E;;

(* Problem 1 *)

let rec filter p l = 
match l with
| [] -> []
| hd::tl ->
	if p hd = true then hd::(filter p tl)
	else filter p tl;;


(* Problem 2 *)

let rec zipper : int list * int list -> int list = fun (a,b)->
match a with
| [] -> b
| hd::tl -> match b with
	| [] -> hd::(zipper (tl,[]))
	| h::t -> hd::h::(zipper (tl,t));;


(* Problem 3 *)

let composition ((f,g) : (int -> int) * (int -> int)) (x : int) = f (g x);;

let rec iter (n,f) =
if n=0 then fun (x : int) -> x
else composition (iter (n-1,f),f);;


(* Problem 4 *)

type aexp = 
  | Const of int 
  | Var of string 
  | Power of string * int 
  | Times of aexp list
  | Sum of aexp list;;

let rec diff(axep, x) = match axep with
  | Const n -> Const 0
  | Var y -> if x=y then Const 1 else Const 0
  | Power (z,m) -> if x=z then (if m=2 then Times [Const m; Var x] else Times [Const m; Power (x,m-1)]) else Const 0
  | Times (h::t) -> Sum [Times (diff (h,x)::t); Times [h; diff (Times t,x)]]
  | Times [] -> Const 0
  | Sum (hd::tl) -> Sum [diff (hd,x); diff (Sum tl,x)] 
  | Sum [] -> Const 0;;


(* Problem 5 *)

type exp = X
  | INT of int
  | ADD of exp * exp
  | SUB of exp * exp
  | MUL of exp * exp
  | DIV of exp * exp
  | SIGMA of exp * exp * exp;;

let rec calculator e = match e with
  | X -> raise E
  | INT a -> a
  | ADD (a,b) -> (calculator a) + (calculator b)
  | SUB (a,b) -> (calculator a) - (calculator b)
  | MUL (a,b) -> (calculator a) * (calculator b)
  | DIV (a,b) -> (calculator a) / (calculator b)
  | SIGMA (a,b,c) -> if (calculator a)>(calculator b) then 0 else eval (c,a) + calculator(SIGMA (ADD (a,INT 1),b,c))

and eval (e,v) = match e with
  | X -> calculator v
  | INT a -> a
  | ADD (a,b) -> eval (a,v) + eval (b,v)
  | SUB (a,b) -> eval (a,v) - eval (b,v)
  | MUL (a,b) -> eval (a,v) * eval (b,v)
  | DIV (a,b) -> eval (a,v) / eval (b,v)
  | SIGMA (a,b,c) -> if (eval (a,v))>(eval (b,v)) then 0 else eval (c,INT(eval (a,v))) + calculator(SIGMA (ADD (INT(eval (a,v)),INT 1),INT(eval (b,v)),c));;
