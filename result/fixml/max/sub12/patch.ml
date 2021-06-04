let rec max l =
  match l with
  | [] -> 0
  | hd :: tl -> if hd < max tl then if tl = [] then hd else max tl else hd
