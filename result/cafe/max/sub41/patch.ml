let rec max (lst : int list) : int =
  match lst with
  | [] -> min_int
  | __s3 :: __s4 -> if __s3 > max __s4 then __s3 else max __s4
