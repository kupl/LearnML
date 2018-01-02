let rec fact n = if n = 0 then 1 else if n = 1 then 1 else n * fact (n-1)
;;
let rec f : int -> int -> int
= fun n1 n2 -> fact (n1) / ( fact(n2) * fact(n1 - n2));;
