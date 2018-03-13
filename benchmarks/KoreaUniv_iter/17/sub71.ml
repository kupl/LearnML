(* problem 3*)

let compose : (int -> int) -> (int -> int) -> (int -> int)
= fun f g -> (fun n -> f(g(n)))

let rec iter_helper : int -> (int -> int) -> (int -> int) -> (int -> int)
= fun n prev f ->
  if n = 0 then prev
  else iter_helper (n-1) (compose prev f) f

let rec iter : int * (int -> int) -> (int -> int)
= fun (n,f) ->
  if n = 0 then (fun x -> x)
  else if n < 0 then raise (Failure ("ValueError: n must be a non-negative integer."))
  else iter_helper (n-1) f f