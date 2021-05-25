let get_first t =
  match t with
  | (x, _, _) -> x

let get_second t =
  match t with
  | (_, x, _) -> x

let get_third t =
  match t with
  | (_, _, x) -> x

let rec sigma (a, b, f) =
  if a > b then 0
  else if a = b then f(a)
  else sigma(a+1, b, f) + f(a)
