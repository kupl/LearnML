let smallest_divisor : int -> int
= fun n -> 
  let p = 2 in
    let rec check p = 
      if p * p > n then n
      else if n mod p = 0 then p
      else check (p + 1)
    in
    if p < 2 then 1
    else check 2;;