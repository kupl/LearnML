type formula = 
  | TRUE
  | FALSE
  | NOT of formula
  | ANDALSO of formula * formula
  | ORELSE of formula * formula
  | IMPLY of formula * formula
  | LESS of expr * expr
and expr = NUM of int
  | PLUS of expr * expr
  | MINUS of expr * expr

let rec count a = 
  match a with
    |NUM n -> n
    |PLUS (m,n) -> count m + count n
    |MINUS (m,n) -> count m - count n

let rec eval a =
  match a with
    |TRUE -> true
    |FALSE -> false
    |NOT nota -> not(eval nota)
    |ANDALSO (aa, ab) -> eval aa && eval ab
    |ORELSE (aa, ab) -> eval aa || eval ab
    |IMPLY (aa, ab) -> 
      if(eval aa && not(eval ab)) then false else true 
    |LESS (expm, expn) -> 
      if(count(expm) < count(expn)) then true else false

