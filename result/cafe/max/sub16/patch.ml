let rec max (l : int list) : int =
  match l with [] -> min_int | hd :: tl -> if hd >= max tl then hd else max tl
