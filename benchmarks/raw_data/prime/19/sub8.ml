let prime : int -> bool
= fun n -> 
  let rec test x y = 
    match y with
      | 1 -> true    
      | _ -> (x mod y <> 0) && test x (y-1)
    in match n with
    | 0 | 1 -> false
    | _ -> test n (n-1) 
;;


