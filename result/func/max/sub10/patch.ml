let rec max (l : int list) : int =
  match l with
  | [ __s3 ] -> 0 + __s3
  | hd :: tl -> if hd > max tl then hd else max tl
  | [] -> -999
