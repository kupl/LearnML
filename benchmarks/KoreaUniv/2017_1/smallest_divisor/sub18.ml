(* problem 2*)
let smallest_divisor : int -> int
  = fun n -> 
    if n <= 2 then n
    else
      let rec test n2 i = 
        if i*i > n2 then n2
        else if n2 mod i = 0 then i
        else test n2 (i+1) in test n 2;;