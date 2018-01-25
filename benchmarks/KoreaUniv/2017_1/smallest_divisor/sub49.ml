(* problem 2*)

let smallest_divisior : int -> int = fun n ->
  let rec div : int -> int -> int = fun n r ->
    if r*r > n then n else
      if n mod r = 0 then r
      else div n (r + 1)
  in div n 2;;