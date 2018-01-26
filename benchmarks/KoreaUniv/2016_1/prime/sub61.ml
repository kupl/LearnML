let rec prime : int -> bool
= fun n ->
let rec dividing d =
d * d > n || (n mod d != 0 && dividing (d+1)) in
n != 1 && (dividing 2) (* TODO *)
