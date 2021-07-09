let rec max (lst : int list) : int =
  match lst with [] -> 0 | h :: t -> if h > max t then h else max t
