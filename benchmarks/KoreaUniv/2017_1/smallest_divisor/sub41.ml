(* problem 2*)

let smallest_divisor : int -> int
= fun n ->
    if n mod 2 == 0 then 2
    else let rec loop r d =
        if n mod d == 0 then d
        else
            if d >= r then 0
            else loop r (d + 2)
        in
            let result =
                loop (int_of_float (sqrt (float_of_int n))) 3 in
                if result == 0 then n
                else result