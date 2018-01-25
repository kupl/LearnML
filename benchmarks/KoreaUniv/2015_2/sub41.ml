(* Programmed by Dong Hyun Koo - 2009210036 *)

(*********************)
(* Problem 1: filter *)
(*********************)
let rec filter pred lst = match lst with
  | [] -> []
  | hd::tl -> if (pred hd) then hd::(filter pred tl) else (filter pred tl)
    
(*********************)
(* Problem 2: zipper *)
(*********************)
let rec zipper : int list * int list -> int list
=fun (a,b) -> match b with
  | [] -> a 
  | bhd::btl ->
    (match a with
    | [] -> b 
    | ahd::atl -> ahd::bhd::(zipper (atl, btl)))

(*******************)
(* Problem 3: iter *)
(*******************)
let rec iter : int * (int -> int) -> (int -> int)
=fun (n,f) -> match n with
  | 0 -> (fun x -> x) 
  | _ -> (fun x ->  (iter ((n-1), f)) (f x))

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
  | Const(b) -> Const 0
  | Var(a) -> if x<>a then Const 0 else Const 1
  | Power(a, b) ->
    if x<>a then Const 0
    else if b=1 then diff (Var a, x)
    else Times[Const b; Power(a, (b-1))]
  | Times(lst) -> (match lst with
    | [] -> Const 0 
    | hd::tl -> Times[hd; diff(Sum(tl), x)])
  | Sum(lst) -> (match lst with
    | [] -> Const 0
    | hd::tl -> Sum[diff(hd, x); diff(Sum(tl), x)])

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
=fun e -> 0
