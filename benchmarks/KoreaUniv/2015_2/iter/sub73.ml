let rec iter : int * (int -> int) -> (int -> int)
=fun (n,f) ->
  if n < 0 then raise (Failure ("first parameter should not be negative"))
           else match n with
                | 0 -> (fun x -> x)
                | _ -> (fun x -> iter( (n - 1), f) (f x))
