let iter (n, f) =
  let rec combine_functions f1 f2 n =
    if n = 0 
      then fun x -> x
      else if n = 1
        then f1
        else combine_functions (fun x -> f1 (f2 x)) f2 (n - 1)
  in
  combine_functions f f n