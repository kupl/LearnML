let rec max (l : int list) : int =
  match l with [] -> min_int | h :: t -> if h > max t then h else max t
