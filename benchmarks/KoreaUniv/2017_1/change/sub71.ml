(* problem 8*)
let rec reverse : int list -> int list
= fun l -> fold (fun x a -> a @ [x]) l []

let rec change : int list -> int -> int
= fun coins amount ->
  if amount = 0 then 1
  else if amount < 0 then 0
  else match coins with
       | [] -> 0
       | _ -> let rev_coins = reverse coins in
              let largest = List.hd rev_coins in
              let remains = List.tl rev_coins in
              (change remains amount) + (change coins (amount - largest))