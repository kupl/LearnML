(*
  1. you can modify the given function specifications as recursive.
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

let diff : aexp * string -> aexp
  = fun (exp, var) -> raise NotImplemented

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
  = fun mob -> raise NotImplemented (* TODO *)

let rec adder e =
      match e with
      |SimpleBranch(n1,n2),SimpleBranch(n3,n4)->n2 + n4      
      |SimpleBranch(n1,n2),CompoundBranch(n3,e)->n2 + (adder e)
      |CompoundBranch(n1,e),SimpleBranch(n2,n3)->(adder e) + n3
      |CompoundBranch(n1,e1),CompoundBranch(n2,e2)->(adder e1)+(adder e2)
  
let balanced : mobile -> bool
  = fun mob -> match mob with
        |SimpleBranch(n1,n2),SimpleBranch(n3,n4)->(n1+n2) = (n3+n4)
        |SimpleBranch(n1,n2),CompoundBranch(n3,e)->(n1+n2) = (n3+(adder e))
        |CompoundBranch(n1,e),SimpleBranch(n2,n3)->(n1+(adder e)) = (n2+n3)
        |CompoundBranch(n1,e1),CompoundBranch(n2,e2)->
                                       (n1+(adder e1)) = (n2+(adder e2))

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

let rec sigma s f func e x
 = if f <= s then
   (func e s)
   else
   (func e f) + (sigma s (f-1) func e x)
      
let rec solver e m =
    match e with
    |X -> m
    |INT n -> n
    |ADD(e1,e2)-> (solver e1 m) + (solver e2 m)
    |SUB(e1,e2)-> (solver e1 m) - (solver e2 m)
    |MUL(e1,e2)-> (solver e1 m) * (solver e2 m)
    |DIV(e1,e2)-> (solver e1 m) / (solver e2 m)
    |SIGMA(e1,e2,e3)-> 
     sigma (solver e1 0) (solver e2 0) solver e3 (solver e1 0)

 (* let calculator : exp -> int
  = fun exp -> raise NotImplemented   TODO *)
  
let calculator : exp -> int
  = fun exp -> (solver exp 0)

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
(*
  let check : exp -> bool
  = fun exp -> raise NotImplemented  TODO *)
(*
let check : exp -> bool
= fun exp -> 

let rec checker exp var = match exp with
                  |V x = x
                  |P(x,e) -> if x = var then true else checker e1 x
                  |C(e1,e2) ->e1 
*)

end


