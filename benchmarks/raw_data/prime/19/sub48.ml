let prime : int -> bool
= fun n -> let y = (n-1) in
    let rec dvable n y = 
      match y with 
      | 1 -> true
      | _ -> if (n mod y) <> 0 then (dvable n (y-1)) else false
      in dvable n y;;

