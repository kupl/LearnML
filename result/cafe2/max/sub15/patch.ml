let rec max (lst : int list) : int =
  match lst with [] -> min_int | h :: t -> if h > max t then h else max t
