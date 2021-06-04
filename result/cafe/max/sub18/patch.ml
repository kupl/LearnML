let big (a : int) (b : int) : int = if a > b then a else b

let rec max (lst : int list) : int =
  match lst with [] -> -100000 | hd :: tl -> List.fold_left big hd tl
