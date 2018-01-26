(* problem 5*)
type exp = X
         | INT of int
         | ADD of exp * exp
         | SUB of exp * exp
         | MUL of exp * exp
         | DIV of exp * exp
         | SIGMA of exp * exp * exp

let rec calculator : exp -> int
= fun e -> 
  let rec eval e v =
  match e with
  | X -> v
  | INT n -> n
  | ADD (e1, e2) -> 
    let v1 = eval e1 v 
    in let v2 = eval e2 v 
    in v1 + v2
  | SUB (e1, e2) -> 
    let v1 = eval e1 v 
    in let v2 = eval e2 v 
    in v1 - v2
  | MUL (e1, e2) -> 
    let v1 = eval e1 v 
    in let v2 = eval e2 v 
    in v1 * v2
  | DIV (e1, e2) -> 
    (match e2 with 
    | INT 0 -> raise (Failure (" cannot be divided by zero"))
    | _ -> let v1 = eval e1 v 
      in let v2 = eval e2 v 
      in v1 / v2)
  | SIGMA(x1, x2, e) -> 
    let v1 = eval x1 v
    in let v2 = eval x2 v
    in let rec sigma_inter x = 
      if x = v1 then eval e x
      else eval e x + sigma_inter (x-1) 
    in sigma_inter v2
  in eval e 0
