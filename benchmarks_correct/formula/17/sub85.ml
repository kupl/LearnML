type expr = 
  | NUM of int
  | PLUS of expr * expr
  | MINUS of expr * expr
        
let rec inteval x = match x with
  | NUM (a) -> a
  | PLUS (a, b) -> inteval(a) + inteval(b)
  | MINUS (a, b) -> inteval(a) - inteval(b)

type formula = 
  | TRUE
  | FALSE
  | NOT of formula
  | ANDALSO of formula * formula
  | ORELSE of formula * formula
  | IMPLY of formula * formula
  | LESS of expr * expr

let rec eval x: bool = match x with
  | TRUE -> true
  | FALSE -> false
  | NOT (a) -> if (eval a) then false else true 
  | ANDALSO (a, b) -> if (eval a && eval b) then true else false
  | ORELSE (a, b) -> if (not (eval a) && not (eval b)) then false else true
  | IMPLY (a, b) -> if (eval a && not (eval b)) then false else true
  | LESS (a, b) -> inteval a < inteval b 
