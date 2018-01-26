(*
  1. You can modify the given function specifications as recursive.
  2. Do not modify the function names or types.
  3. It is free to define any helper functions.
*)

exception NotImplemented

(*********************)
(*     Problem 1     *)
(*********************)
module Problem1 = struct
  type aexp = 
  | Const of int
  | Var of string
  | Power of string * int
  | Times of aexp list
  | Sum of aexp list

  let rec diff : aexp * string -> aexp
  = fun (exp, var) ->

    let rec t2 vl var = 
      match vl with
      [] -> []
      | hd::tl -> [Times( [diff(hd, var)]@tl ); Times( hd::(t2 tl var) )]

    in let rec t el c vl var = 
      match el with 
      [] -> Times[Const(c); Sum(t2 vl var)] 
      | hd::tl -> 
        match hd with
        | Const(x) -> t tl (c * x) vl var
        | a -> t tl c (vl @ [a]) var

    in let rec s el l var = 
      match el with
      | [] -> Sum(l)
      | hd::tl -> s tl (l@[diff(hd, var)]) var 

    in match exp with
    | Const (x) -> Const(0)
    | Var (x) -> if x = var then Const(1) else Const(0)
    | Power (x, y) -> 
      if x = var then 
        if y = 0 then Const(0) 
        else if y = 1 then diff (Var(x), var)
        else if y = 2 then Times[Const(2); Var(x)]
        else Times[Const(y); Power(x, y-1)]
      else Const(0)
    | Times (l) ->  t l 1 [] var 
    | Sum (l) -> s l [] var
end

(*********************)
(*     Problem 2     *)
(*********************)
module Problem2 = struct
  type mobile = branch * branch
  and branch = 
  | SimpleBranch of length * weight
  | CompoundBranch of length * mobile
  and length = int
  and weight = int

  let balanced : mobile -> bool
  = fun mob -> 
    let rec calW (b1, b2) =
        (match b1 with 
        | SimpleBranch(l, w) -> w
        | CompoundBranch(l, m) -> calW m) +
        (match b2 with 
        | SimpleBranch(l, w) -> w
        | CompoundBranch(l, m) -> calW m)
    in let cal b = 
      match b with 
      | SimpleBranch(l, w) -> l * w 
      | CompoundBranch(l, m) -> l * calW m
    in match mob with
    | (b1, b2) -> cal b1 == cal b2
end

(*********************)
(*     Problem 3     *)
(*********************)
module Problem3 = struct
  type exp =
  | X
  | INT of int
  | ADD of exp * exp
  | SUB of exp * exp
  | MUL of exp * exp
  | DIV of exp * exp
  | SIGMA of exp * exp * exp

  let rec calculator : exp -> int
  = fun exp ->
  let rec calc e n =   
    match e with
    | X -> n
    | INT(x) -> x
    | ADD(x, y) -> (calc x n) + (calc y n)
    | SUB(x, y) -> (calc x n) - (calc y n)
    | MUL(x, y) -> (calc x n) * (calc y n)
    | DIV(x, y) -> (calc x n) / (calc y n)
    | SIGMA (p, q, x) -> let rec sum i j x = if i <= j then (calc x i) + sum (i+1) j x else 0
        in sum (calc p 0) (calc q 0) x
  in calc exp 0
end

(*********************)
(*     Problem 4     *)
(*********************)
module Problem4 = struct
  type exp =
  | V of var
  | P of var * exp
  | C of exp * exp
  and var = string

  let check : exp -> bool
  = fun exp -> 
  let rec m x l = 
    match l with
    | [] -> false
    | hd :: tl -> if hd = x then true else m x tl
  in let rec find ex l = 
    match ex with
    | P(x, y) -> find y (x :: l) 
    | C(x, y) -> find x l && find y l
    | V(x) -> m x l
  in find exp []
end