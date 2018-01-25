(********************)
(* helper functions *)
(********************)
let compound f g = fun x -> f (g x)
let rec element f s = 
match s with
| [] -> []
| hd::tl -> (f hd)::(element f tl) 
(*********************)
(* Problem 1: filter *)
(*********************)
let rec filter pred lst =
match lst with
| [] -> []
| hd::tl -> if (pred hd) then hd::(filter pred tl) else (filter pred tl) 
(*********************)
(* Problem 2: zipper *)
(*********************)
let rec zipper : int list * int list -> int list
=fun (a,b) ->
match a,b with 
| [],[] -> [] 
| [],_ -> b 
| _,[] -> a
| hda::tla, hdb::tlb -> hda::hdb::zipper(tla,tlb)
(*******************)
(* Problem 3: iter *)
(*******************)
let rec iter : int * (int -> int) -> (int -> int)
=fun (n,f) -> 
if n=0 then (fun x->x) else if n=1 then f else compound f (iter(n-1,f))
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
| Const _ -> Const 0
| Var v -> if x=v then Const 1 else Const 0
| Power (p,n) ->
  if (x=p) then (if not (n=1) then Times [Const n; Power(p,n-1)] else Const 1) else Const 0
| Times [] -> Const 0
| Times (hd::tl) -> if tl = [] then diff (hd,x)
else Sum [(if diff (hd,x) = Const 0 then Const 0 else Times (diff(hd,x)::tl)); Times [hd; diff (Times tl,x)]]
|Sum list -> Sum (element (fun l -> diff(l,x)) list)
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
=fun e ->
match e with
| X -> raise (Failure "Type Error")
| INT n -> n
| ADD (x,y) -> (calculator x) + (calculator y)
| SUB (x,y) -> (calculator x) - (calculator y)
| MUL (x,y) -> (calculator x) * (calculator y)
| DIV (x,y) -> (calculator x) / (calculator y)
| SIGMA (x,y,z) -> if calculator x > calculator y  then 0 else cal (z,x) + calculator (SIGMA (ADD(x, INT 1),y,z))
and cal (z,x) =
match z with
| X -> calculator x
| INT n -> n
| ADD (a,b) -> cal (a,x) + cal(b,x)
| SUB (a,b) -> cal (a,x) - cal(b,x)
| MUL (a,b) -> cal (a,x) * cal(b,x)
| DIV (a,b) -> cal (a,x) / cal(b,x)
| SIGMA (a,b,c) -> calculator (SIGMA (a,b,c))



