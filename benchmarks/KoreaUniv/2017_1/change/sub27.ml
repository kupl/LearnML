(*problem 8*)
let rec reverse: int list ->int list =fun lst-> match lst with
| []->[]
|hd::tl -> reverse tl @ [hd]
let rec remove: int->int list ->int list=fun a lst-> match lst with
[]->[]
|hd::tl -> if hd= a then tl else hd::(remove a tl)
let rec change: int list->int->int= fun coins amount->
if amount=0 then 1
else if amount < 0 then 0
else match reverse coins with
[]->0
|hd::tl-> change coins (amount-hd) + change (remove hd coins) amount

