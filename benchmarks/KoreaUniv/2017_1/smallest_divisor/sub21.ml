(* problem 2*)
let rec ans : int -> int -> int
= fun i n ->
  if i*i >n then -1
  else begin
    if ((n mod i) = 0) then i
    else ans (i+2) n
  end

let smallest_divisor : int -> int
= fun n ->
  if n mod 2 = 0 then 2
  else begin
    let answer  = ans 3 n in
    if answer = -1 then n
    else answer
  end