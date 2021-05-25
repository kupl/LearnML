let rec iter : int * (int -> int) -> (int -> int)
= fun (n,f) ->
  fun a ->
    if n = 0 then (fun x -> x) a
    else iter(n-1,f)(f a);;
