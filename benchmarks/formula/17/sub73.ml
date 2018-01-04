type expr = NUM of int
          | PLUS of expr * expr
          | MINUS of expr * expr

type formula = TRUE
              | FALSE
              | NOT of formula
              | ANDALSO of formula * formula
              | ORELSE of formula * formula
              | IMPLY of formula * formula
              | LESS of expr * expr

let rec eval : formula -> bool = fun f ->
  let rec calcexpr : expr -> expr = fun x ->
    let rec numtoint : expr -> int = fun n ->
      match n with
      | NUM i -> i
      | PLUS (a, b) -> ((numtoint a) + (numtoint b))
      | MINUS (a, b) -> ((numtoint a) - (numtoint b)) in
    let rec inttonum : int -> expr = fun i ->
      NUM i in
    inttonum (numtoint x)
  in
  match f with
  | TRUE -> true
  | FALSE -> false
  | NOT f1 -> not (eval f1)
  | ANDALSO (f1, f2) -> (eval f1) && (eval f2)
  | ORELSE (f1, f2) -> (eval f1) || (eval f2)
  | IMPLY (f1, f2) -> (not (eval f1)) || (eval f2)
  | LESS (e1, e2) -> (calcexpr e1) < (calcexpr e2)
