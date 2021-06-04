let rec sigma (f : int -> int) (a : int) (b : int) : int =
  match (b, b, f) with
  | __s5, __s6, __s7 -> if a > __s5 then 0 else __s7 a + sigma __s7 (a + 1) __s5
