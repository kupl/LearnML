let compose f g x = f (g x)

let rec iter : int * (int -> int) -> int -> int =
 fun (n, f) ->
  match n with
  | 0 -> fun __x__ -> __x__
  | 1 -> f
  | _ -> compose f (iter (n - 1, f))
