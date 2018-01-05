exception Error of string
let rec sigma :(int * int * (int->int)) -> int =
  fun abf ->
    try (
      match abf
      with (a,b,f) ->
        (
          match (compare a b)
          with 0 -> (f a)
            | 1 -> raise (Error("a is bigger than b"))
            | _ -> (f a)+(sigma(a+1,b,f))
        )
    )
    with Error(s)-> raise (Error s)
      | _ ->raise (Error "cake is a lie")
