exception InputError

let rec prime_sub : int -> int -> bool
= fun a b ->
  if b = 0 then false
  else if b = 1 then true
  else if (a mod b = 0) then false
  else prime_sub a (b - 1)

let rec prime : int -> bool
= fun n -> 
  if n <= 0 then raise InputError
  else prime_sub n (n - 1)
