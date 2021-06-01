exception Not_minus of string
let rec iter (n, f) =
  if n < 0
  then
    raise (Not_minus "n cannot be less than 0")
  else
    match n = 0 with
    | true -> (fun x -> x)
    | false -> (fun x -> iter (n - 1, f) (f x))
