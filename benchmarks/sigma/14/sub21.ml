let rec sigma eq =
  match eq with
  | (a, b, f) when (a <= b) -> (f a) + (sigma (a + 1, b, f))
  | _ -> 0 ;;
