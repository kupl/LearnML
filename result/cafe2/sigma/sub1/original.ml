exception Error of string

let rec sigma (f : int -> int) (a : int) (b : int) : int =
  if a > b then raise Error "Invalid interval" else f a + sigma f (a + 1) b
