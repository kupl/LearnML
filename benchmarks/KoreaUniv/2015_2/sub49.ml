(*********************)
(* Problem 1: filter *)
(*********************)
let rec filter pred lst =
  match lst with 
  [] -> []
  | h::t -> if(pred h) then h::(filter pred t) else filter pred t

(*********************)
(* Problem 2: zipper *)
(*********************)
let rec zipper : int list * int list -> int list
=fun (a,b) ->
match a with
[] -> b
| h::t -> h::(zipper (b,t))

(*******************)
(* Problem 3: iter *)
(*******************)

let rec iter : int * (int -> int) -> (int -> int)
=fun (n,f) -> 
if(n=0) then fun x-> x
else if(n=1) then f
else fun x -> f(iter(n-1,f) x) 

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
Const a -> Const 0
| Var s -> diff (Power(s,1),x)
| Power (s,a) -> if(s=x) then Times [Const a; Power (s,a-1)] else Const 0
| Times lst -> 
(match lst with
[] -> Const 1 
| h::t -> if(t=[]) then diff(h,x) 
else Sum [Times [h; diff (Times t,x)]; Times (diff (h,x)::t)] ) 

| Sum lst -> 
match lst with 
[] -> Const 0
| h::t -> if(t=[]) then diff (h,x)
else Sum [diff (h,x); diff (Sum t,x)]


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


let rec exptoint : exp*int-> int
=fun (e,a) ->
( match e with
X -> a
| INT x -> x
| ADD (x,y) -> exptoint (x,a)+ exptoint (y,a)
| SUB (x,y) -> exptoint (x,a)- exptoint (y,a)
| MUL (x,y) -> exptoint (x,a)* exptoint (y,a)
| DIV (x,y) -> exptoint (x,a)/ exptoint (y,a)
| SIGMA(x,y,f) -> 
(
  if(exptoint (x,a)<exptoint (y,a)) then exptoint (f,exptoint (x,a)) + exptoint (SIGMA(INT ((exptoint (x,a))+1),y,f),(exptoint (x,a))+1)
  else if (exptoint (x,a)=exptoint (y,a)) then exptoint (f,exptoint(x,a))
  else 0 )
)

let calculator : exp -> int
=fun e -> exptoint (e,0)