(* let rec sigma ((a: int), (b: int), (f: int -> int)): int = *)
let rec sigma (a, b, f) =
  match a > b with
  | true -> 0
  | false -> (f a) + (sigma (a + 1, b, f))
