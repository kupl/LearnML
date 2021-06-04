let rec max : int list -> int =
 fun lst ->
  match lst with
  | [] -> 0
  | h :: t -> if h > max t then h else if t = [] then h else max t
