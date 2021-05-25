let rec checkPrime : int * int -> bool
=  fun(num, limitNum) ->
  if limitNum = 1 then true
  else if num mod limitNum = 0 then false
  else checkPrime(num, (limitNum - 1))
;;

let prime : int -> bool
= fun n -> 
  match n with 
    | 1 -> true (*checks the base case of 1 *)
    | 2 -> true (*other special case as 2 is the only prime number*)
    | _ -> checkPrime(n, int_of_float(sqrt(float_of_int n)))
;;

prime 10;;

