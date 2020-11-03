type formula=True
            |False
            |Not of formula
            |AndAlso of formula*formula
            |OrElse of formula*formula
            |Imply of formula*formula
            |Equal of exp*exp
and exp=Num of int
        |Plus of exp*exp
        |Minus of exp*exp

let rec eval_exp:exp->int=fun(a)->
  match a with
  |Num b->b
  |Plus (b,c)->eval_exp(b)+eval_exp(c)
  |Minus (b,c)->eval_exp(b)-eval_exp(c)

let rec eval:formula->bool=fun(a)->
  match a with
  |True->true
  |False->false
  |Not b->not(eval b)
  |AndAlso (b,c)->eval(b)&&eval(c)
  |OrElse (b,c)->eval(b)||eval(c)
  |Imply (b,c)->eval c||not(eval b)
  |Equal (b,c)->eval_exp(b)=eval_exp(c)
