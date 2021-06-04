let rec sigma : (int -> int) -> int -> int -> int =
 fun func a b -> if b = a then func a else func a + sigma func (a + 1) b
