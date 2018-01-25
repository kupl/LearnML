(* problem 2*)
let smallest_divisor : int -> int =
  fun n -> if (n mod 2) = 0 then 2 else let i = 2 in
    let rec compare a b =
      if a >= (b * b) then
        if (a mod b) = 0 then b else compare a (b+1)
      else a in
    compare n i;;