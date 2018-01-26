let rec prime : int -> bool
= fun n ->
  let rec divisible: int * int -> bool
  = fun (n1, n2) ->
    if n1 = n2 then true
    else if n1 mod n2 = 0 then false
    else divisible (n1, (n2+1))
  in
    if n = 2 then true
    else divisible (n, 2)
;;
