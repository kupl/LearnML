let prime : int -> bool
= fun n -> match n with
  | 2 -> true
  | _ -> let rec divisor x = x * x > n || n mod x <> 0 && divisor (x+1) in n <> 1 && divisor 2;;
