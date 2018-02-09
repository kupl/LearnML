let rec double: ('a -> 'a) ->'a ->'a
  = fun f a ->
  f(f a)
