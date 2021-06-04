let rec max : int list -> int =
 fun lst ->
  match lst with [] -> -999999999 | h :: t -> if h > max t then h else max t
