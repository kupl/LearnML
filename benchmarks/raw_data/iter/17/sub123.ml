let rec iter x y =
  match x with
  | (n,f) ->
    if n > 0 then
      iter (n-1,f) (f y)
    else
      y
