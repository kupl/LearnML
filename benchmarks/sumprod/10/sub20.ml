exception Error of string
let sumprod : (int * int -> float) * int * int -> float =
  fun matrix_n_k -> 
    let rec prod : (int * int -> float) * int * int -> float =
      fun matrix_i_j ->
        (
          match matrix_i_j
          with (matrix,i,j) ->
            if j = 0
            then 1.0
            else (matrix (i,j)) *. (prod (matrix,i,j-1))
        )
    in
    let rec sumprod : (int*int ->float) *int *int -> float =
      fun matrix_n_k -> 
        (
          match matrix_n_k
          with (matrix,n,k) ->
            if n=0
            then 0.0
            else (sumprod (matrix,n-1,k)) +. (prod (matrix,n,k))
        )
    in
      try (
        match matrix_n_k
        with (matrix,n,k) ->
          if n <= 0
          then raise (Error "n is not positive")
          else
            if k <= 0
            then raise (Error "k is not positive")
            else sumprod matrix_n_k
      )
      with Error s -> raise (Error s)
        | _ -> raise (Error "cake is a lie")
