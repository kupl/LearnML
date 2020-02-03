(*********************)
(*     Problem 1     *)
(*********************)
let rec max : int list -> int
  = fun lst ->
  let rec maxx : int -> int -> int
  	= fun a b ->
  	if a > b then a else b in
  match lst with
  |m::[] -> m
  |m::n -> maxx m (max n);;
 