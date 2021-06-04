let __s3 (__s4 : int) (__s5 : int) : int = if __s4 > __s5 then __s4 else __s5

let rec max (lst : int list) : int =
  match lst with __s6 :: __s7 -> List.fold_left __s3 __s6 lst
