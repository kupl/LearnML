(*********************)
(* Promble 1: filter *)
(*********************)
let rec filter pred lst =
match lst with
|[] -> []
|h::t -> if (pred h) then h::(filter pred t)
else filter pred t

(*********************)
(* Problem 2: zipper *)
(*********************)
let rec zipper : int list * int list -> int list
=fun (a,b) -> 
match (a,b) with
|(_,[]) -> a
|([],_) -> b
|(h1::t1,h2::t2) -> h1::(h2::zipper(t1,t2))
(*******************)
(* Problem 3: iter *)
(*******************)

let rec iter : int * (int -> int) -> (int -> int)
=fun (n,f) -> 
if n=0 then (fun a->a) 
else (fun a -> iter(n-1, f) (f a)) 

(*********************)
(* Problem 4: Diff   *)
(*********************)
type aexp = 
  | Const of int 
  | Var of string 
  | Power of string * int 
  | Times of aexp list
  | Sum of aexp list;;

let rec diff : aexp * string -> aexp
=fun (aexp,x) ->
match aexp with
|Const(a) -> Const 0
|Var(y) -> if x=y then Const 1 else Const 0
|Power(y, a) ->
	 if x<>y then Const 0
	 else if a=1 then diff (Var y, x)
	 else Times [Const a;Power(y,(a-1))]
|Times(a::b) -> Times [a;diff (Sum(b),x)]
|Sum(l) ->
	match l with
	|[] -> Const 0
	|h::t -> Sum [(diff (h,x));(diff (Sum(t),x))]
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
= fun e ->
let rec fnc : exp -> (int -> int)
= fun s ->
match s with 
|X -> (fun x -> x)
|INT(i) -> (fun x -> i)
|ADD(a,b) -> (fun x -> ((fnc a) x) + ((fnc b) x))
|MUL(a,b) -> (fun x -> ((fnc a) x) * ((fnc b) x))
|SUB(a,b) -> (fun x -> ((fnc a) x) - ((fnc b) x))
|DIV(a,b) -> (fun x -> ((fnc a) x) / ((fnc b) x))
in let rec result a b f =
if a=b then f a
else (f a)+(result (a+1) b f)
in match e with
|SIGMA(INT(a),INT(b),c) -> result a b (fnc c) 
