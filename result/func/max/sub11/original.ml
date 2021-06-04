let rec max (l : int list) : int =
  match l with [] -> 0 | hd :: tl -> if hd > max tl then hd else max tl
