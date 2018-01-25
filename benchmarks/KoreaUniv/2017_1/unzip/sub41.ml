(* problem 7*)

let rec unzip : ('a * 'b) list -> 'a list * 'b list
= fun lst ->
    let merge (x1, y1) (x2, y2) = (x1 :: x2, y1 :: y2) in
    match lst with
    | (a, b) :: tail -> merge (a, b) (unzip tail)
    | _ -> ([], [])