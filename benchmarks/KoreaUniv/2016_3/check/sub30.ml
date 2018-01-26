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
  match exp with
  | Const a -> Const 0
  | Var a-> if var = a then Const 1 else Var a
  | Power (a, b) -> if var = a then Times [Const b; Power (a, b-1)] else Power (a, b)
  | Times a -> 
  begin
    match a with
    | hd::tl -> Sum[Times ([diff (hd, var)]@tl); Times [hd;diff (Times tl, var)]]
    | []-> Const 0
  end
  | Sum a->
  begin
    match a with
    | hd::tl -> Sum[diff (hd, var); diff (Sum tl,var)]
    | []-> Const 0 
  end
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
  let rec mobileWeight : mobile -> int
  = fun mob ->
    match mob with
    | (b1,b2) -> 
      match b1 with
      | SimpleBranch(l1,w1) -> 
        match b2 with
        | SimpleBranch(l2,w2) -> w1 + w2
        | CompoundBranch(l2,m2) -> l1 + mobileWeight m2 
      | CompoundBranch(l1,m1) ->
        match b2 with
        | SimpleBranch(l2,w2) -> mobileWeight m1 + w2
        | CompoundBranch(l2,m2) -> mobileWeight m1 + mobileWeight m2

  let balanced : mobile -> bool

  = fun mob -> 
    match mob with
    | (b1,b2) -> 
      match b1 with
      | SimpleBranch(l1,w1) -> 
        match b2 with
        | SimpleBranch(l2,w2) -> l1*w1 = l2*w2
        | CompoundBranch(l2,m2) -> l1*w1 = l2*mobileWeight m2
      | CompoundBranch(l1,m1) ->
        match b2 with
        | SimpleBranch(l2,w2) -> l1*mobileWeight m1 = l2*w2
        | CompoundBranch(l2,m2) -> l1*mobileWeight m1 = l2*mobileWeight m2

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
    let rec varSet : exp->int->int
    = fun exp value ->
    match exp with
    | X-> value
    | INT a -> a
    | ADD (a,b) -> varSet a value + varSet b value
    | SUB (a,b) -> varSet a value - varSet b value 
    | MUL (a,b) -> varSet a value * varSet b value
    | DIV (a,b) -> varSet a value / varSet b value
  in match exp with
  | INT a -> a
  | ADD (a,b) -> calculator a + calculator b
  | SUB (a,b) -> calculator a - calculator b 
  | MUL (a,b) -> calculator a * calculator b
  | DIV (a,b) -> calculator a / calculator b
  | SIGMA (a,b,e) -> 
    let rec sigma : int -> int -> exp ->int 
    = fun start last exp3 -> if start <= last then (varSet exp3 start) + sigma (start+1) last exp3 else 0
  in sigma (calculator a) (calculator b) e
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
  = fun exp -> raise NotImplemented
  
end

