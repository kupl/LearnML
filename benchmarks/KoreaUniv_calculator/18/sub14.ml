type exp = 
  X
  | INT of int
  | ADD of exp * exp
  | SUB of exp * exp
  | MUL of exp * exp
  | DIV of exp * exp
  | SIGMA of exp * exp * exp
  
 
exception NoValue;;
 
let rec calculator : exp -> int
= fun exp ->
  let rec assignValue : exp * exp -> int
  = fun (e1, e2) ->
    match e1 with
    | X -> calculator e2
    | INT a -> a
    | ADD (a, b) -> (assignValue (a, e2)) + (assignValue (b, e2))
    | SUB (a, b) -> (assignValue (a, e2)) - (assignValue (b, e2))
    | MUL (a, b) -> (assignValue (a, e2)) * (assignValue (b, e2))
    | DIV (a, b) -> (assignValue (a, e2)) / (assignValue (b, e2))
    | SIGMA (a, b, c) -> calculator (SIGMA (INT (assignValue (a, e2)), INT (assignValue (b, e2)), c))
  in
  match exp with
  | X -> raise NoValue
  | INT a -> a
  | ADD (a, b) -> (calculator a) + (calculator b)
  | SUB (a, b) -> (calculator a) - (calculator b)
  | MUL (a, b) -> (calculator a) * (calculator b)
  | DIV (a, b) -> (calculator a) / (calculator b)
  | SIGMA (a, b, c) -> let start = (calculator a) and finish = (calculator b) in
                         if (start > finish) then 0 else
                           (assignValue (c, a)) + calculator (SIGMA (ADD (a, INT 1), b, c))
;;


(*
We need to define a new function [assignValue : exp * exp -> int] that will perform the assignment operation for each SIGMA iteration.

I couldn't quite figure out how to implement an assignment function, so instead chose to choose a match-and-return style function.

1. [assignValue : exp * exp -> int] receives a two-element tuple with each element of exp type. More specifically, the first element is the
  equation for the SIGMA operation, and the second element is the value that we must assign to variable X.
2. We perform recursion on the elements until they are of the most primitive form.
3. When we reach the primitive form for each element, we will be left with either both elements being [INT x] or X.
4. If one is X, then we simply return the other. If both are [INT x], then simply return one.
*)



