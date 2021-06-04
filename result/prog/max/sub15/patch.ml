let rec max (lst : int list) : int =
  match lst with
  | [] -> -999999999
  | [ __s3 ] -> __s3
  | h :: t -> if h > max t then h else max t
