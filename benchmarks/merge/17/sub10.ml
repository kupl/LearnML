let rec merge ((xs: int list), (ys: int list)): int list = 
    match (xs, ys) with
    | ([], _) -> ys
    | (_, []) -> xs
    | (x::xs', y::ys') when x > y -> x :: (merge (xs', ys))
    | (x::xs', y::ys') -> y :: (merge (xs, ys'))

(*let merged = merge ([3;2;1], [4;3;2]);;*)
(*List.iter print_int merged;;*)
(*print_endline ""*)
