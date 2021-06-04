let rec max l =
  match l with [] -> 0 | hd :: tl -> if hd < max tl then max tl else hd
