(* problem 8*)
let rec change : int list -> int -> int
= fun coins amount ->
  if (amount < 0) then 0 else
  let rec iterate : int -> int -> int -> int -> int list -> int
  = fun coin amount sum idx coins ->
    if idx = 0 then (sum + change coins amount)
    else iterate coin amount (sum + change coins (amount-(idx*coin))) (idx-1) coins
  in
  match coins with
  | [] -> if amount = 0 then 1 else 0
  | hd::tl -> iterate hd amount 0 (amount/hd) tl
;;