(* problem 2*)

let smallest_divisor : int -> int
= fun n -> let rec a n r = if r*r > n then n
                                      else if (n mod r = 0) then r
                                           else a n (r+1)
  in a n 2