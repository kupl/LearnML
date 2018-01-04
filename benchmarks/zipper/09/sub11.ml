let rec zipper ((a:int list), (b:int list)) =
  if (a = []) then b
  else (List.hd a)::(zipper (b, (List.tl a)))
