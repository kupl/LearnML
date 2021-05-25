let prime : int -> bool
= fun n -> (*TODO*)
    let rec isPrime m d = 
      match d with
        |  1 -> true    
        | _ -> (m mod d <> 0) && isPrime m (d-1)
      in match n with
      |  1 -> false
      | _ -> isPrime n (n-1);;


(*let prime : int -> bool*)
(*= fun n -> (*TODO*)*)
