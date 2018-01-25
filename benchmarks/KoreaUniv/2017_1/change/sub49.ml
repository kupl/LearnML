(* problem 8*)

let rec change : int list -> int -> int = fun coins amount -> 
  match amount with
  | 0 -> 1
  | _ -> match coins with
    | [] -> 0
    | hd::tl -> (change tl amount) + if amount < hd then 0 else change coins (amount -hd);;
