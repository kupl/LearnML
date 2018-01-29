(* problem 7*)
type digit = ZERO | ONE
type bin = digit list

let bmul : bin -> bin -> bin
= fun b1 b2 -> (* TODO *)
let to_int b =
  let rec to_int_impl _b n =
    match _b with
    | [] -> n
    | hd::tl ->
    if hd = ONE then to_int_impl tl (n * 2 + 1)
    else to_int_impl tl (n * 2) in
  to_int_impl b 0 in
let to_bin i =
  let rec to_bin_impl b _i =
    if _i = 0 then b
    else
      if _i mod 2 = 1 then to_bin_impl (ONE::b) (_i / 2)
      else to_bin_impl (ZERO::b) (_i / 2) in
  if i = 0 then [ZERO]
  else to_bin_impl [] i in
to_bin ((to_int b1) * (to_int b2));;
