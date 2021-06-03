(* let rec sigma ((a: int), (b: int), (f: int -> int)): int = *)
let rec sigma f a b =
  match a > b with
  | true -> 0
  | false -> (f a) + (sigma f (a+1) b)
