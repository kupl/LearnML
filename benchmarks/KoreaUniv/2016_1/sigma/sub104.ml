
let rec sigma (f : int -> int) (a : int) (b : int) =
 if a = b then f a
 else f a + sigma f (a+1) b;;
