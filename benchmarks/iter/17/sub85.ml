let iter: int * ('a -> 'a) -> ('a -> 'a) = fun (n, f) ->
  let rec _iter (_n: int) (acc: 'a -> 'a) =
    if _n <= 0
      then acc
      else _iter (_n-1) (fun x -> acc (f x))
  in _iter n (fun x -> x)
