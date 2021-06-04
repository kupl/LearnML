let max_f (a : int) (b : int) : int = if a > b then a else b

let rec max (lst : int list) : int =
  match lst with [] -> -99999 | hd :: tl -> List.fold_left max_f hd tl
