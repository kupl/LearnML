let rec zipper : int list * int list -> int list
=fun (a,b) -> match b with
  | [] -> a 
  | bhd::btl ->
    (match a with
    | [] -> b 
    | ahd::atl -> ahd::bhd::(zipper (atl, btl)))
