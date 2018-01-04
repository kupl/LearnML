let iter : int * ('a -> 'a) -> ('a -> 'a) = fun (n, f) ->
  let rec r_iter = fun (n, f, r) ->
    if(n == 0) then r
    else r_iter(n - 1, f, (fun x -> f(r(x)))) in
  let id = (fun x -> x) in
  if(n == 0) then id
  else r_iter(n, f, id)
