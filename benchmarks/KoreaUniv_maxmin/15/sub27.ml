let rec max : int list -> int
=fun l -> 
  match l with
  | [] -> 0
  | hd :: [] -> hd
  | hd :: tl ->
      let r = max tl in 
      if hd > r then
        hd
      else
        r
;;

let rec min : int list -> int
=fun l -> 
  match l with
  | [] -> 0
  | hd :: [] -> hd
  | hd :: tl ->
      let r = min tl in
      if hd < r then
        hd
      else
        r
;;
