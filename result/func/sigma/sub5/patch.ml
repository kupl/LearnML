let rec sigma (f : int -> int) (a : int) (b : int) : int =
  match (b, b) with
  | __s5, __s6 ->
      if a = __s5 then f __s5
      else if __s5 > __s5 then 0
      else f a + sigma f (a + 1) __s5
