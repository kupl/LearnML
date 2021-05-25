
let iter (n, f) =
  fun x -> (
      let rec iter_f y n =
        match n with
          | 0 -> y
          | _ -> iter_f (f y) (n - 1)
      in
        iter_f x n
    )

