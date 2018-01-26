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
      = fun (aexp, var) -> 
      match aexp with
        |Const cst1 -> Const 0
        |Var var1 -> if var1 = var then Const 1 else Const 0
      |Power(str, int1) -> begin
          match int1 with
            |0 -> Const 0
            |1 -> if str = var then Const 1 else Const 0
            |_ -> Times[Power(str, int1-1);Const int1]
        end
        |Sum sum_lst -> begin
          match sum_lst with
            |[] -> Const 0
          |[hd] -> diff(hd, var)
            |hd ::tl  -> Sum[diff(hd,var); diff(Sum tl, var)]
        end
        |Times tm_lst-> 
        match tm_lst with 
          |[] -> Const 1
          |[hd] -> diff(hd, var)
          |hd :: tl -> Sum[Times[diff(hd, var);Times tl]; Times[hd; diff(Times tl, var)]]
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
  let rec weights : mobile -> int
  = fun wt -> match wt with
  |(SimpleBranch(l1,w1) ,SimpleBranch(l2,w2)) -> w1+w2
  |(SimpleBranch(l1,w1) ,CompoundBranch(l2, mob2)) -> w1 + weights mob2
  |(CompoundBranch(l1, mob1) ,SimpleBranch(l2,w2)) -> w2 + weights mob1
  |(CompoundBranch(l1, mob1) ,CompoundBranch(l2, mob2))-> weights mob1 + weights mob2
in
  match mob with
  |(SimpleBranch(l1,w1) ,SimpleBranch(l2,w2)) -> if l1*w1 = l2*w2 then true else false
  |(SimpleBranch(l1,w1) ,CompoundBranch(l2, mob2)) -> if l1*w1 = l2*(weights mob2) then true else false
  |(CompoundBranch(l1,mob1) ,SimpleBranch(l2,w2)) ->if l2*w2 = l1*(weights mob1) then true else false
  |(CompoundBranch(l1,mob1) ,CompoundBranch(l2, mob2))-> if l1*(weights mob1) = l2*(weights mob2) then true else false

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

  let calculator : exp -> int
  = fun exp -> 
  let inttoexp : int -> exp
  = fun int1 -> match int1 with
  | x -> INT x in
  let rec meaning : exp -> exp -> int
  = fun exp n-> match exp with
  | X -> meaning n n
  | INT a -> a
  | ADD(a, b) -> meaning a n + meaning b n
  | SUB(a, b) -> meaning a n - meaning b n
  | MUL(a, b) -> (meaning a n) * (meaning b n)
  | DIV(a, b) -> (meaning a n) / (meaning b n)
  | SIGMA(a, b ,c) ->   
    let rec sigma : exp -> int -> int -> int
    = fun k x1 y1->
    if x1 < y1 then meaning k (inttoexp x1) + sigma k (x1+1) y1 else meaning k (inttoexp x1) 
    in 
  sigma c (meaning a n) (meaning b n)
in
  meaning exp (inttoexp 0)
 
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
    let rec deletehd : string -> string list -> bool 
    = fun var l -> 
    match l with
      | [] -> false
      | hd :: tl -> if hd = var then true 
                    else deletehd var tl
    in 

    let rec isbound : exp -> string list -> bool
    = fun exp lst ->
      match exp with
        | P(var, exp1) -> isbound exp1 (var :: lst) 
        | C(exp1, exp2) -> isbound exp1 lst && isbound exp2 lst
        | V(var) -> deletehd var lst
    in 

    isbound exp []
end