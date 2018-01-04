type formula = TRUE
              |FALSE
              |NOT of formula
              |ANDALSO of formula * formula
              |ORELSE of formula * formula
              |IMPLY of formula * formula
              |LESS of expr * expr
and expr = NUM of int
          |PLUS of expr * expr
          |MINUS of expr * expr

let rec calc var_expr = 
  match var_expr with
    NUM int_var -> int_var
    |PLUS (expr1, expr2) -> (calc expr1) + (calc expr2)
    |MINUS (expr1, expr2) -> (calc expr1) - (calc expr2)

let rec eval var_form =
  match var_form with
    TRUE -> true
    |FALSE -> false
    |NOT form1 -> if (eval form1) == true then false else true
    |ANDALSO (form1, form2) -> (eval form1) && (eval form2)
    |ORELSE (form1,form2) -> (eval form1) || (eval form2)
    |IMPLY (form1,form2) -> if (eval form1) == false then true else (eval form2)
    |LESS (expr1,expr2) -> if (calc expr1) < (calc expr2) then true else false
