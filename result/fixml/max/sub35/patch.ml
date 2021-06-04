let rec max : int list -> int =
 fun lst ->
  match lst with
  | [] -> 0
  | hd :: tl -> if hd < max tl && max [] < max tl then max tl else hd
