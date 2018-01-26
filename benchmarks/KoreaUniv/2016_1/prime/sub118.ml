let rec prime : int -> bool
= fun n -> let n = abs n in
    let rec ndiv d = d*d > n || (n mod d <> 0 && ndiv (d+1)) in n <> 1 && ndiv 2
