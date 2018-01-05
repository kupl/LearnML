let rec sigma' ((a: int), (b: int), (f: int->int), (subSum: int)): int =
    if a = b then f a + subSum
    else sigma' (a + 1, b, f, subSum + f a)

let sigma ((a: int), (b: int), (f: int->int)): int =
    if a > b then 0
    else sigma' (a, b, f, 0)

;; sigma (1, 2, (fun x -> x * x)) |> print_int
; print_endline ""
