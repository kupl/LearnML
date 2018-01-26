(* problem 7*)
type digit = ZERO | ONE
type bin = digit list

let bmul : bin -> bin -> bin
= fun b1 b2 ->
  let bintoint : bin -> int
  = fun binary ->
    let rec bi : bin -> int -> int
    = fun b sum ->
      match b with
      | hd::tl -> bi tl ((sum*2)+(if hd=ONE then 1 else 0))
      | [] -> sum
    in
    bi binary 0
  in
  let inttobin : int -> bin
  = fun integer ->
    let rec ib : int -> bin -> bin
    = fun i b ->
      if i > 0 then (ib (i/2) ((if i mod 2 = 0 then ZERO else ONE)::b))
      else b
    in
    ib integer []
  in
  inttobin ((bintoint b1)*(bintoint b2))
;;