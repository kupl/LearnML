let rec max (lst : int list) : int =
  match lst with
  | [] -> -100000
  | hd :: tl -> if hd > max tl then hd else max tl
