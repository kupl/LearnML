let rec checknum x y = match y with
  | 1 -> true 
  | _ -> (x mod y <> 0) && checknum x (y -1);;
let rec prime : int -> bool
= fun n -> match n with
  |0|1 -> false
  |_ -> checknum n (n-1);;
  prime 4;;
  prime 3;;
