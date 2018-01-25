(* problem 8*)

let rec change : int list -> int -> int
= fun coins amount ->
    match coins with
    | [] -> if amount = 0 then 1
            else 0
    | hd::tl -> if hd > amount then change tl amount
                else (change coins (amount-hd)) + (change tl amount)
