(* problem 8*)

let rec change_get coins len amount
= if amount = 0 then 1
  else if amount<0 then 0
  else if len = 0 then 0
  else (change_get coins (len-1) amount)+(change_get coins len(amount-List.nth coins (len-1)));;

let change coins amount
= let len = List.length coins in
  change_get coins len amount;;
