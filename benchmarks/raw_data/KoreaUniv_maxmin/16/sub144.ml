(*********************)
(*     Problem 1     *)
(*********************)
let rec max : int list -> int
  = fun lsh ->
  let rec maxh : int -> int -> int
  	= fun a b ->
  	if a > b then a else b in
  match lsh with
  |m::[] -> m
  |m::n -> maxh m (max n);;

 