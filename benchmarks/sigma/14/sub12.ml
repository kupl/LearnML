let sigma: (int * int * (int -> int)) -> int =
  fun (fromIndex, toIndex, f) ->
    let rec sigma_aux: (int * int) -> int =
      fun (currentIndex, result) ->
        if currentIndex > toIndex then result
        else sigma_aux (currentIndex + 1, result + (f currentIndex))
    in
    sigma_aux (fromIndex, 0)
