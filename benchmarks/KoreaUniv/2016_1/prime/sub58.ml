let rec primeTest n i =
if i = 1 then true
else if (n mod i) = 0 then false
else primeTest n (i - 1)

let rec prime : int -> bool
= fun n ->
if n < 2 then false
else primeTest n (n - 1)
