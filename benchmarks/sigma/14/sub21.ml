let rec sigma eq =
  match eq with
  | (a, b, f) -> if (a<=b) then (f a) + (sigma (a + 1, b, f)) else 0
  | _ -> 0 ;;
