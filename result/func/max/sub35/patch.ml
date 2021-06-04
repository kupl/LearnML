let rec max (lst : int list) : int =
  match lst with
  | [] -> 0
  | hd :: tl ->
      if hd < max tl && tl = [] then hd else if hd < max tl then max tl else hd
