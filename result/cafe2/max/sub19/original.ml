let rec max2 (a : int) (b : int) : int = if a >= b then a else b

let rec max (lst : int list) : int =
  match lst with [] -> -999999999 | hd :: tl -> max2 hd (max tl)
