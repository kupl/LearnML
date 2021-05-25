 let prime n =
    let rec xq x m = match m with
        | 1 -> true    
        | _ -> (x mod m <> 0) && xq x (m-1)
    in match n with
    | 0 | 1 -> false
    | _ -> xq n (n-1) ;;
    
prime 2 ;;
prime 3 ;;
prime 4 ;;
prime 17;;
    