let comb f g x = f (g x) ;;

let rec iter : int * (int -> int) -> (int -> int) =
  fun (n, f) ->
  if n = 0 then fun x-> x
  else fun x -> comb f (iter(n-1, f)) x;;