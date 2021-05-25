let rec iter : int * (int -> int) -> (int -> int) =
  fun (n, f) ->
    let f2 x =
      match n with
        0 -> f x
      | 1 -> f x
      | _ -> f (iter (n - 1, f) x)
    in
    f2;;
  
