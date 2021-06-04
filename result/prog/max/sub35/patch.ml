let rec max (lst : int list) : int =
  match lst with
  | [] -> 0
  | [ __s3 ] -> __s3
  | hd :: tl -> if max tl < hd then hd else max tl
