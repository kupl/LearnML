exception Error of string;;

let sumprod (matrix, n, k) =
  if n < 0 || k < 0 then raise (Error "n < 0 || k < 0") else
  let rec fold sum f i e =
    if i < 1 then assert false else
    if i > e then sum else fold (sum +. f i) f (i + 1) e
  in
  fold 0.0 (fun x -> fold 0.0 (fun y -> matrix (x, y)) 1 k) 1 n;;
