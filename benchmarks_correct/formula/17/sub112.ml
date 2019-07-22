type formula=TRUE
            |FALSE
            |NOT of formula
            |ANDALSO of formula*formula
            |ORELSE of formula*formula
            |IMPLY of formula*formula
            |LESS of expr*expr
and expr=NUM of int
        |PLUS of expr*expr
        |MINUS of expr*expr

let rec eval_expr:expr->int=fun(a)->
  match a with
  |NUM b->b
  |PLUS (b,c)->eval_expr(b)+eval_expr(c)
  |MINUS (b,c)->eval_expr(b)-eval_expr(c)

let rec eval:formula->bool=fun(a)->
  match a with
  |TRUE->true
  |FALSE->false
  |NOT b->not(eval b)
  |ANDALSO (b,c)->eval(b)&&eval(c)
  |ORELSE (b,c)->eval(b)||eval(c)
  |IMPLY (b,c)->eval c||not(eval b)
  |LESS (b,c)->eval_expr(b)<eval_expr(c)
