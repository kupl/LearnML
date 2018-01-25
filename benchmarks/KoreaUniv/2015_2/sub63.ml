(*********************)
(* Problem 1: filter *)
(*********************)
let rec filter pred lst = match lst with 
	[] -> []
	| (hd :: tl) -> if (pred hd) then hd :: (filter pred tl)
					else (filter pred tl)

(*********************)
(* Problem 2: zipper *)
(*********************)
let rec zipper : int list * int list -> int list
=fun (a,b) -> match a,b with
  | [],b -> b
  | a,[] -> a
  | (x::xs),(y::ys) -> x :: y :: (zipper (xs,ys))
  
(*******************)
(* Problem 3: iter *)
(*******************)

let rec iter : int * (int -> int) -> (int -> int)
=fun (n,f) -> 
if n = 0 then (fun x -> x)
else f

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
|Const a -> Const 0
|Var x -> Const 1
|Power ("x", a) -> ( match a with
 1 -> Var "x"
 |_ -> Times[Const a; Power ("x", a-1)]
)
|Times l -> ( match l with
 h::t -> Sum[Times[diff(h,"x"); Sum t];Times[h;diff(Sum t,"x")]]
)
|Sum l -> (match l with
 h::t -> Sum[diff(h,"x");diff(Sum t,"x")]
 |a ::_ -> diff(a,"x")
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
let calculator : exp -> int
=fun e -> 0 (* TODO *)
