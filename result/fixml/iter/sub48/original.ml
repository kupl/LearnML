let rec iter : int * (int -> int) -> int -> int =
 fun (n, f) ->
  match n with 1 -> f | _ -> let g = iter (n - 1, f) in

                             fun x -> f (g x)
