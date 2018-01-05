type formula =
  | TRUE
  | FALSE
  | NOT of formula
  | ANDALSO of formula * formula
  | ORELSE of formula * formula
  | IMPLY of formula * formula
  | LESS of expr * expr
and expr =
  | NUM of int
  | PLUS of expr * expr
  | MINUS of expr * expr

let rec eval (f : formula) : bool =
  match f with
  | TRUE  -> true
  | FALSE -> false
  | NOT subf -> not (eval subf)
  | ANDALSO (sf1, sf2) -> eval sf1 && eval sf2
  | ORELSE  (sf1, sf2) -> eval sf1 || eval sf2
  | IMPLY   (sf1, sf2) -> not (eval sf1) || eval sf2
  | LESS (e1, e2) ->
    let rec evalexpr (e : expr) : int =
      match e with
      | NUM i -> i
      | PLUS  (se1, se2) -> evalexpr se1 + evalexpr se2
      | MINUS (se1, se2) -> evalexpr se1 - evalexpr se2 in
    evalexpr e1 < evalexpr e2
