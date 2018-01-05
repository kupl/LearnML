exception Error of string;;

let sigma (a, b, f) =
  if a > b then raise (Error "a > b") else
  let rec fold sum x =
    if x < a then assert false else
    if x > b then sum else fold (sum + f x) (x + 1)
  in
  fold 0 a b;;
