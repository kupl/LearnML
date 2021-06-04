let rec max l =
  match l with
  | [] -> 0
  | hd :: tl -> if hd >= max tl then hd else if tl = [] then hd else max tl
