type formula = 
    TRUE
  | FALSE
  | NOT of formula
  | ANDALSO of formula * formula
  | ORELSE of formula * formula
  | IMPLY of formula * formula
  | LESS of expr * expr

and expr =
    NUM of int
  | PLUS of expr * expr
  | MINUS of expr * expr


let rec eval f =
  let rec eval_num e =
    match e with
    | NUM e -> e
    | PLUS (e1, e2) -> eval_num e1 + eval_num e2
    | MINUS (e1, e2) -> eval_num e1 - eval_num e2
  in
  match f with
  | TRUE -> true
  | FALSE -> false
  | NOT fp -> not (eval fp)
  | ANDALSO (fp1, fp2) -> (eval fp1) && (eval fp2)
  | ORELSE (fp1, fp2) -> (eval fp1) || (eval fp2)
  | IMPLY (fp1, fp2) -> not (eval fp1) || (eval fp2)
  | LESS (e1, e2) -> (eval_num e1) < (eval_num e2)
