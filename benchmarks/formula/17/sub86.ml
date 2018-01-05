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
    
    let rec expr2int : expr -> int = function
      | NUM(a) -> a
      | PLUS(a, b) -> expr2int(a) + expr2int(b)
      | MINUS(a, b) -> expr2int(a) - expr2int(b)
    
    
    let rec eval : formula -> bool = function
      | TRUE -> true
      | FALSE -> false
      | NOT(a) -> not(eval(a))
      | ANDALSO(a, b) -> eval(a) && eval(b)
      | ORELSE(a,b) -> eval(a) || eval(b)
      | IMPLY(a,b) -> not(eval(a)) || eval(b)
      | LESS(a, b) -> expr2int(a) < expr2int(b)
        