(* problem 3*)
type formula =
    True
  | False
  | Var of string
  | Neg of formula
  | And of formula * formula
  | Or of formula * formula
  | Imply of formula * formula
  | Iff of formula * formula

let rec logic : formula -> bool
= fun f ->
  match f with
    | True -> true
    | False -> false
    | Var str -> true
    | Neg form -> 
      if (form = True) then false
      else if (form = False) then true
      else if (logic form) then true
      else false
    | And (form1, form2) ->
      if((form1 = Neg form2)||(form2 = Neg form1)) then false
      else if ((logic form1)&&(logic form2)) then true
      else false
    | Or (form1, form2) ->
      if ((logic form1)||(logic form2)) then true
      else false
    | Imply (form1, form2) ->
      if ((logic form1)&&(logic form2)) then true
      else if (((logic form1)=true)&&((logic form2)=false)) then false
      else if (((logic form1)=false)&&((logic form2)=true)) then true
      else true
    | Iff (form1, form2) ->
      if ((logic form1)&&(logic form2)) then true
      else if(((logic form1) = false) && ((logic form2) = false)) then true
      else false 

let sat : formula -> bool
= fun f -> (* TODO *)
  match f with
    | True -> true 
    | False -> false 
    | Var str -> true
    | Neg form -> logic f
    | And (form1, form2) -> logic f
    | Or (form1, form2) -> logic f
    | Imply (form1, form2) -> logic f
    | Iff (form1, form2) -> logic f