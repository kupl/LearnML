let rec prime : int -> bool
= fun n -> let k = int_of_float (floor (sqrt (float_of_int n))) in
let rec modif n k =
if k = 1 then true
else if k >= 2 && n mod k = 0 then false
else if k = 2 && n mod k <> 0 then true
else modif n (k-1) in
if n >= 2 then modif n k
else false;;
