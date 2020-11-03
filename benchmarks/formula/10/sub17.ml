type formula = True
              |False
              |Not of formula
              |AndAlso of formula * formula
              |OrElse of formula * formula
              |Imply of formula * formula
              |Equal of exp * exp
and exp = Num of int
          |Plus of exp * exp
          |Minus of exp * exp

let rec calc var_exp = 
  match var_exp with
    Num int_var -> int_var
    |Plus (exp1, exp2) -> (calc exp1) + (calc exp2)
    |Minus (exp1, exp2) -> (calc exp1) - (calc exp2)

let rec eval var_form =
  match var_form with
    True -> true
    |False -> false
    |Not form1 -> if (eval form1) == true then false else true
    |AndAlso (form1, form2) -> (eval form1) && (eval form2)
    |OrElse (form1,form2) -> (eval form1) || (eval form2)
    |Imply (form1,form2) -> if (eval form1) == false then true else (eval form2)
    |Equal (exp1,exp2) -> if (calc exp1) = (calc exp2) then true else false
