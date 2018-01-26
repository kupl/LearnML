let rec primeHelper : int -> int -> bool
= fun n divisor ->
if (divisor <= 1) then true
else if ((n mod divisor) = 0) then false
else primeHelper n (divisor - 1);;

let rec prime : int -> bool
= fun n -> primeHelper n (int_of_float (sqrt (float_of_int n)));;
