let rec prime : int -> bool
= fun n -> if n%prime(n-1) = 0 then false else true;;
