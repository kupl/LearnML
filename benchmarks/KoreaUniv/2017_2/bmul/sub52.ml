(* problem 7*)
type digit = ZERO | ONE
type bin = digit list

let rec badd : bin -> bin -> digit -> bin
= fun b1 b2 carry ->
  match (b1, b2, carry) with
  | ([], [], ZERO) -> [ZERO]
  | ([], [], ONE) -> [ONE]
  | (ZERO::tl, [], ZERO) -> ZERO :: (badd tl [] ZERO)
  | (ZERO::tl, [], ONE) -> ONE :: (badd tl [] ZERO)
  | (ONE::tl, [], ZERO) -> ONE :: (badd tl [] ZERO)
  | (ONE::tl, [], ONE) -> ZERO :: (badd tl [] ONE)
  | ([], ZERO::tl, ZERO) -> ZERO :: (badd [] tl ZERO)
  | ([], ZERO::tl, ONE) -> ONE :: (badd [] tl ZERO)
  | ([], ONE::tl, ZERO) -> ONE :: (badd [] tl ZERO)
  | ([], ONE::tl, ONE) -> ZERO :: (badd [] tl ONE)
  | (ZERO::tl1, ZERO::tl2, ZERO) ->  ZERO :: (badd tl1 tl2 ZERO)
  | (ZERO::tl1, ZERO::tl2, ONE) ->  ONE :: (badd tl1 tl2 ZERO)
  | (ONE::tl1, ZERO::tl2, ZERO) ->  ONE :: (badd tl1 tl2 ZERO)
  | (ONE::tl1, ZERO::tl2, ONE) ->  ZERO :: (badd tl1 tl2 ONE)
  | (ZERO::tl1, ONE::tl2, ZERO) ->  ONE :: (badd tl1 tl2 ZERO)
  | (ZERO::tl1, ONE::tl2, ONE) ->  ZERO :: (badd tl1 tl2 ONE)
  | (ONE::tl1, ONE::tl2, ZERO) ->  ZERO :: (badd tl1 tl2 ONE)
  | (ONE::tl1, ONE::tl2, ONE) ->  ONE :: (badd tl1 tl2 ONE)

let rec bmul_helper : bin -> bin -> bin -> bin
= fun b1 b2 offset ->
  match b1 with
  | [] -> [ZERO]
  | hd::tl -> let imt = (if hd = ONE then offset @ b2 else [ZERO]) in
              badd imt (bmul_helper tl b2 (ZERO::offset)) ZERO

let rec trim_zeros : bin -> bin
= fun b ->
  match b with
  | [] -> []
  | [ZERO] -> [ZERO]
  | [ONE] -> [ONE]
  | hd::tl -> if hd = ONE then b else (trim_zeros tl)

let bmul : bin -> bin -> bin
= fun b1 b2 ->
  let rev_b1 = List.rev b1 in
  let rev_b2 = List.rev b2 in
  let rev_mult = bmul_helper rev_b1 rev_b2 [] in
  let mult_zeros = List.rev rev_mult in
  let mult = trim_zeros mult_zeros in
  mult
