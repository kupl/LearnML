let compose (f : 'b -> 'a) (g : 'c -> 'b) (x : int) = f (g x)

let rec iter ((n : int), (f : int -> int)) : int -> int =
  match n with
  | 0 -> fun (__s14 : int) -> __s14
  | 1 -> f
  | _ ->
      let asdf : int -> int = iter (n - 1, f) in
      compose f asdf
