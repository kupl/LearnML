let dividable : int*int->bool
=fun (a,b) ->
  if a mod b =0 then true else false;;
  
let rec is_prime : int*int -> bool
= fun (n,a) ->
  match a with
  |1 -> true
  |_ -> if dividable(n,a) then false else is_prime(n,a-1);;
  
let prime : int -> bool
= fun n -> 
  try
    is_prime(n,n-1)
  with Division_by_zero -> false;;        

