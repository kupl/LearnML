type lambda = 
  V of var
| P of var * lambda
| C of lambda * lambda
and var = string

let check met = 
  let rec listcheck (str, varlist) = 
    match varlist with
    |[] -> false
    |a::lst -> 
      if(a=str) then true else listcheck(str, lst)
  in
  let rec lambdalistcheck (met, arealist) =
    match met with
    |V a -> listcheck(a, arealist)
    |P(a, m) -> lambdalistcheck(m, a::arealist)
    |C(m1, m2) -> lambdalistcheck(m1, arealist) && lambdalistcheck(m2, arealist)
  in
  lambdalistcheck(met,[])



  
