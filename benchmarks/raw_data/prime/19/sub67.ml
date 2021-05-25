let rec is_prime: int * int -> bool
= fun (n, i) ->
  if i < 2 then true
  else if n mod i = 0 then false
  else is_prime(n, i - 1);;

let prime : int -> bool
= fun n ->
  if n < 2 then false
  else is_prime(n, n - 1);;
