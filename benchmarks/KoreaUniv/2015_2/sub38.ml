(*********************)
(* Problem 1: filter *)
(*********************)
let rec filter pred lst =
match lst with
|[] -> lst
|hd::tl ->
	(match pred hd with
	|true -> hd::filter pred tl
	|_ -> filter pred tl)
	

(*********************)
(* Problem 2: zipper *)
(*********************)
let rec zipper : int list * int list -> int list
=fun (a,b) -> 
match a with
|[] ->
	(match b with
	|[] -> b
	|hd::tl -> hd::zipper([],tl))
|h::t->
	(match b with
	|[] -> h::zipper(t,[])
	|hd::tl->h::hd::zipper(t,tl))
 


(*******************)
(* Problem 3: iter *)
(*******************)
let comp f g = (fun x -> f(g x))


let rec loop : int*(int -> int)*(int -> int) -> (int -> int)
= fun (x,y,z)->
if x!=1 then loop(x-1,y,comp z y) else z


let rec iter : int * (int -> int) -> (int -> int)
=fun (n,f) -> 
match n with
|0 -> f
|_ -> loop(n,f,f)



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
| Const a -> Const 0
| Var a -> Var a
| Power(a,b) -> if b=1 then Var(a) else Times[Const b;Power(a,b-1)]
| Times a-> 
(match a with
|hd::tl ->
	(match hd with
	|Const t -> Times[(Const 0);Var(x)];Times[Const t;Sum[Times [Const 1];Times[Var(x);Const 0]]]
(*diff(Times(tl),x)*)
	|Var t -> Times[Const 1];Times[Var(t);Const 0])
)
| Sum a ->
(match a with
|hd::tl -> Sum[diff(hd,x);diff(Sum(tl),x)]
|_ -> Const 0
)

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

let temp = 0
let v = 0

let rec z_fun : exp*int -> int
= fun(a,b) ->
match a with
|X -> b
|INT x -> x
|ADD(x,y) -> z_fun(x,b)+z_fun(y,b)
|SUB(x,y) -> z_fun(x,b)-z_fun(y,b)
|MUL(x,y) -> z_fun(x,b)*z_fun(y,b)
|DIV(x,y) -> z_fun(x,b)/z_fun(y,b)


let rec calculator : exp -> int
=fun e ->
match e with
|SIGMA(x,y,z) ->if z_fun(x,0)< z_fun(y,0)+1 then z_fun(z,z_fun(x,0))+calculator(SIGMA(INT(z_fun(x,0)+1),y,z)) else 0
|_ -> z_fun(e,0);;
(*
match e with
|X -> v
|INT x -> x
|ADD(x,y) ->calculator x + calculator y
|SUB(x,y) ->calculator x - calculator y
|MUL(x,y) ->calculator x * calculator y
|DIV(x,y) ->calculator x / calculator y
|SIGMA(x,y,z) -> let v = v+calculator x+1 in
if calculator x<(calculator y+1) then temp+(fun x -> calculator z) v+calculator(SIGMA(INT ((calculator x)+1),y,z)) else temp
(*cal(calculator x, calculator y, (fun x-> calculator z),0)*)
 *)
