let rec max : int list -> int
=fun l -> 
  match l with
  | [] -> raise (Failure "Empty list")
  | [a] -> a
  | hd::tl ->
    let maxintl = max tl in
    if maxintl < hd then hd
    else maxintl

let rec min : int list -> int
=fun l ->
  match l with
  | [] -> raise (Failure "Empty list")
  | [a] -> a
  | hd::tl ->
    let minintl = min tl in
    if minintl > hd then hd
    else minintl
