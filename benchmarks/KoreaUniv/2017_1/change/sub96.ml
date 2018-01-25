(*problem 8 *)
let change : int list -> int -> int
= fun coins amount ->
let rec count coins amount idx=
if amount = 0 then 1
else if (amount < 0) || (idx >= List.length coins) then 0
else (count coins amount (idx+1)) + (count coins (amount - (List.nth coins idx)) idx) in count coins amount 0
