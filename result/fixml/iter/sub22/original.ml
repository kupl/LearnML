let compose f g x = f (g x)

let rec iter : int * (int -> int) -> int -> int =
 fun (n, f) ->
  match n with
  | 1 -> f
  | _ ->
      let asdf = iter (n - 1, f) in
      compose f asdf
