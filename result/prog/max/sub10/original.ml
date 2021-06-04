let rec max (l : int list) : int =
  match l with hd :: tl -> if hd > max tl then hd else max tl | [] -> -999
