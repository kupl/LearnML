
let rec iter (n,f) =
  if n = 0 then (fun x -> x)
  else fun x -> f (iter (n-1, f) x)

  (*
let _ = print_int (iter (10, fun x -> 2*x) 1)
*)
