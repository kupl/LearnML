let rec sigma (func : int -> int) (a : int) (b : int) : int =
  if func a = func b then func a else func a + sigma func (a + 1) b
