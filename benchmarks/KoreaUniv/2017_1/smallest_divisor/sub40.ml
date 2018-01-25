(* problem 2 *) 
let rec devisor
= fun n d -> match d with
        | _ -> if (n mod d = 0) then d else (devisor n (d+2))
let smallest_divisor
= fun n ->
    match n with
    | _ -> if (n mod 2 = 0) then 2 else (devisor n 3)