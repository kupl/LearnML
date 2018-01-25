(* problem 2*)

let rec smallest_divisor : int -> int
= fun n -> let rec find_divisor : int -> int -> int
            = fun n test_divisor -> if test_divisor * test_divisor > n then n
                                    else if (n mod test_divisor) = 0 then test_divisor
                                    else find_divisor n (test_divisor+1)
  in find_divisor n 2