(* problem 8*)

let rec change : int list -> int -> int
= fun coins amount ->
  (match amount with
  | 0 -> 1
  | _ ->
    (match coins with
    | [] -> 0
    | hd::tl ->
      if amount < 0 then 0
      else  (change coins (amount-hd))+(change tl amount)
    )
  )