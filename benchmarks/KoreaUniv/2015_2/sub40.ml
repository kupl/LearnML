(*********************)
(* Problem 1: filter *)
(*********************)
let rec filter pred lst
= match lst with
  | [] -> []
  | hd::rest -> if (pred hd) then hd::(filter pred rest) 
    else filter pred rest

(*********************)
(* Problem 2: zipper *)
(*********************)
let rec zipper : int list * int list -> int list
=fun (a,b) -> 
  match a, b with
  | [], y::[] -> [y]
  | x::[], [] -> [x]
  | x::xs, y::ys -> if (x < y) then x::zipper(xs,y::ys)
    else y::zipper(x::xs, ys)

(*******************)
(* Problem 3: iter *)
(*******************)

let rec iter : int * (int -> int) -> (int -> int)
=fun (n,f) -> f(* let cal n f x = f x 
  if n>1 then iter (n-1) f + x
                else f )*)



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
  | Const n -> Const 0 
  | Var chars -> Var chars
  | Power(chars, n) -> if n>2 then Times [Const n; Power(chars, n-1)] else Times [Const n; Var chars]
  | Times lst ->
    begin match lst with
    | [] -> Const 0
    | aexp::tl ->
      (match aexp with
       | Const n -> Const n )
    end
  | Sum lst ->
    begin match lst with
    | [] -> Const 0
    | hd ->  Const 0
    | hd::tl -> Sum [diff(hd,x); (Sum tl)]
    end

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
