  type lambda = V of var
           | P of var * lambda
           | C of lambda * lambda
  and var = string
 
   let check : lambda -> bool
  =fun e -> true
 
let rec exist v lst = match lst with 
| [] -> false
| hd::tl ->  if (v = hd) then true else ( exist v tl) 
 
let rec finding e lst = match e with
| V (v) ->  exist v lst 
| P (v,e) ->  finding e (lst@[v])
| C (e1,e2) -> if ((finding e1 lst) && (finding e2 lst)) then true else false
 
let rec check : lambda -> bool
= fun e -> finding e []
 