(* problem 7*)
type digit = ZERO | ONE
type bin = digit list

let bmul : bin -> bin -> bin
= fun b1 b2 -> (* TODO *)
  (*let x = List.rev b1 and
  let y = List.rev b2 in*)
  let rec helper b1 b2 =
    match b1 with
    |[ZERO] -> [ZERO]
    |[ONE] -> b2
    |[ONE;ZERO] -> b2@[ZERO]
    |h::t -> if h=ZERO then helper t b2 else b2@[ZERO] in
      helper b1 b2
