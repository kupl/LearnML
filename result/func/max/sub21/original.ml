let max_f (a : int) (b : int) : int = if a > b then a else b

let rec max (lst : int list) : int =
  match lst with [] -> -99999 | hd :: tl -> max_f hd (max tl)
