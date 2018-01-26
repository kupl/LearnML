let rec prime : int -> bool
= fun n -> let rec nd i = i * 2 > n || (nd (i+1) && n mod i <> 0) in n <> 0 && n <> 1 && nd 2
