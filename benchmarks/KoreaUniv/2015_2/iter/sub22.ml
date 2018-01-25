let rec iter (n,f) num =
 match n with
  | 0 -> num
  | n -> iter (n-1,f) (f num)
