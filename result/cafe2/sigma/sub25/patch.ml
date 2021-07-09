let rec sigma (func : int -> int) (a : int) (b : int) : int =
  if b = a - 1 then 0 else func a + sigma func (a + 1) b
