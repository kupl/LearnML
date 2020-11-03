(*let rec app l1 l2 =
  match l1 l2 with
    [] []  -> []
    |[] m::n-> m::n
    |hd::tl [] -> hd::tl
    |hd::tl m::n -> 
    if hd > m then m::app tl n ::hd;;
  *)
  
  (*let rec app l1 l2 =
    let ln = List.rev l1 in
    let rec apptr ln l2 acc = match ln with
        | [] -> acc
        | hd::tl -> apptr tl l2 (hd::acc) in
    apptr ln l2 l2
;;*)
let rec app l1 l2 =
  match l1 with
  [] -> l2
  |hd::tl -> hd::app tl l2;;
app [4;5;6;7] [1;2;3;4];;