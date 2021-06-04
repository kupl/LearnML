let big a b = if a > b then a else b

let rec max : int list -> int =
 fun lst ->
  match lst with
  | [] -> -100000
  | hd :: tl -> big hd (if tl = [] then hd else max tl)
