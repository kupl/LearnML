(*8*)

let rec change_help coins m amount =
 let th coins m = List.nth coins m in
  if amount=0 then 1
      else if amount<0 then 0
        else if (m<=0&& amount>=1) then 0
          else ((change_help coins (m-1) amount) +(change_help coins m (amount-(th coins (m-1)))));;

let change : int list->int->int = fun coins amount ->
  let m = List.length coins in
  if amount<0 then 0 else  change_help coins m amount;;

