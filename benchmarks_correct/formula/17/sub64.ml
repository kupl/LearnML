type formula = TRUE
              | FALSE
              | NOT of formula
              | ANDALSO of formula * formula
              | ORELSE of formula * formula
              | IMPLY of formula * formula
              | LESS of expr * expr
and expr = NUM of int
          | PLUS of expr * expr
          | MINUS of expr * expr

let rec expr2int (n: expr) : int =
  match n with
  | NUM x -> x
  | PLUS (x, y) -> (expr2int x) + (expr2int y)
  | MINUS (x, y) -> (expr2int x) - (expr2int y)

let rec eval (f: formula) : bool =
  match f with
  | TRUE -> true
  | FALSE -> false
  | NOT f' -> (match (eval f') with
              | true -> false
              | false -> true)
  | ANDALSO (a, b) -> if ((eval a) == false) then false
                      else if ((eval b) == false) then false
                      else true
  | ORELSE (a, b) -> if ((eval a) == true) then true
                     else if ((eval b) == true) then true
                     else false
  | IMPLY (a, b) -> if ((eval a) == false) then true
                    else if ((eval b == true)) then true
                    else false
  | LESS (x, y) -> if (expr2int x) < (expr2int y) then true
                   else false
