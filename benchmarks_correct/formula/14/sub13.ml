exception EvalError

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

let rec eval: formula -> bool =
  fun f ->
    let rec eval_expr: expr -> expr =
      fun e ->
        match e with
        | NUM n ->
            NUM n
        | PLUS (NUM n1, NUM n2) ->
            NUM (n1 + n2)
        | PLUS (e1, e2) ->
            eval_expr (PLUS ((eval_expr e1), (eval_expr e2)))
        | MINUS (NUM n1, NUM n2) ->
            NUM (n1 - n2)
        | MINUS (e1, e2) ->
            eval_expr (MINUS ((eval_expr e1), (eval_expr e2))) in
    match f with
    | TRUE ->
        true
    | FALSE ->
        false
    | NOT f1 ->
        not (eval f1)
    | ANDALSO (f1, f2) ->
        (eval f1) && (eval f2)
    | ORELSE (f1, f2) ->
        (eval f1) || (eval f2)
    | IMPLY (f1, f2) ->
        (not (eval f1)) || (eval f2)
    | LESS (e1, e2) ->
        match ((eval_expr e1), (eval_expr e2)) with
        | (NUM n1, NUM n2) ->
            n1 < n2
        | _ ->
            raise EvalError
