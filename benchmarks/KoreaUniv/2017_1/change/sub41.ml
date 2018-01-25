(* problem 8*)

let rec change : int list -> int -> int
= fun coins amount ->
    if amount == 0 then 1
    else if amount < 0 then 0
    else match coins with
    | c :: rest -> let rec loop = fun n ->
        if n < 0 then 0
        else change rest (amount - n * c) + loop (n - 1)
    in loop (amount / c)
    | _ -> 0
