  (* problem 2*)

  let smallest_divisor : int -> int
  = fun n -> 
    let rec asdf n k =
    if n mod k = 0 then k
    else if k>(n/2) then n
    else asdf n (k+1)
    in asdf n 2