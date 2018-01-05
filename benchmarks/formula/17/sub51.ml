type formula = TRUE | FALSE | NOT of formula
             | ANDALSO of formula * formula
             | ORELSE of formula * formula
             | IMPLY of formula * formula
             | LESS of expr * expr
           and expr = NUM of int
             | PLUS of expr * expr
             | MINUS of expr * expr

let rec eval : formula -> bool = fun f ->
  let rec eval_expr : expr -> int = fun e ->
    match e with
    | NUM a -> a
    | PLUS (b, c) -> (eval_expr b) + (eval_expr c)
    | MINUS (d, e) -> (eval_expr d) - (eval_expr e) in
  match f with 
  | TRUE -> true
  | FALSE -> false
  | NOT a -> not (eval a)
  | ANDALSO (b, c) -> (eval b) && (eval c)
  | ORELSE (d, e) -> (eval d) || (eval e)
  | IMPLY (f, g) -> (
    let eval_f : bool = (eval f) in
    not eval_f || (eval_f && eval g))
  | LESS (h, i) -> ((eval_expr h) < (eval_expr i))

let print_bool b = 
  match b with
  | true -> print_endline "true"
  | false -> print_endline "false"
