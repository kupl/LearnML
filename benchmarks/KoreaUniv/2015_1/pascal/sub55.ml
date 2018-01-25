(* Problem 1 *)
let pascal : int * int -> int
=fun (x,y) ->
        let rec factorial n = if n <= 0 then 1
                                         else n * factorial (n - 1) in
        (factorial x) / ((factorial y) * (factorial (x - y)))
