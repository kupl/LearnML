
  type exp =
  | V of var
  | P of var * exp
  | C of exp * exp
  and var = string

  let rec compre1 :var*exp -> bool
  =fun (exp1,exp2)->
  match exp1,exp2 with
  | var,V vari->
   if var=vari then true
   else false
  | (var,P(vari,expi))->
   if (var=vari) &&compre1(var,expi)then true
   else false
  | (var,C(exp1,exp2))->
  compre1(var,exp1)||compre1(var,exp2)

  let rec compre2 :exp*exp -> bool
  =fun (exp1,exp2)->
  match exp1,exp2 with
  | V vari,V varj->
  if  vari=varj then true
  else false
  | V vari,P(varj,expj)->
  if (vari=varj)&&compre1(vari,expj)
  | V vari,C(expji,expjj)->
  | P(vari,expi),P(varj,expj)->
  | P(vari,expi),C(expji,expjj)->
  | C(expii,expij),C(expji,expjj)->



  let check : exp -> bool
  = fun exp->
match exp with
| V var-> true
| P(var,exp)-> compre1(var,exp)
| C(exp1,exp2)-> compre2(exp1,exp2)



