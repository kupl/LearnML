let prime n = 
  let rec isZero x a = match a with
    | 1 -> true
    | _ -> (x mod a <> 0) && isZero x (a-1)
  in match n with
  | 0 -> false
  | 1 -> false
  | _ -> isZero n (n-1) ;;
  