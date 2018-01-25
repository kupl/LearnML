(* problem 2*)
let smallest_divisor : int -> int
= fun n ->
  let rec it = fun c ->
    if (c*c > n) then n
    else if (n mod c = 0) then c
    else it(c+1)
  in
  it 2
;;