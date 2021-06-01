let get_first some_tuple =
  match some_tuple with
  | (x, _, _) -> x

let get_second some_tuple = 
  match some_tuple with
  | (_, x, _) -> x

let get_third some_tuple =
  match some_tuple with
  | (_, _, x) -> x

let rec sigma f a b  =
  let params = (a, b, f) in
  let a = get_first params in
  let b = get_second params in
  let f = get_third params in
  if a > b then 0
  else if a = b then f a 
  else (f a) + (sigma f (a+1) b)
