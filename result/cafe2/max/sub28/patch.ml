let rec max (lst : int list) : int =
  match lst with
  | [] -> min_int
  | hd :: tl -> if hd > max tl then hd else max tl
