(* problem 7 *)
type digit = ZERO | ONE
type bin = digit list

let rec bmul : bin -> bin -> bin = fun b1 b2 ->
  let rec blen : bin -> int = fun b ->
    (match b with
      | [] -> 0
      | _::tl -> 1 + blen tl
    )
  in
  let rec zadd : bin -> int -> bin = fun b n ->
    (match n with
      | 0 -> b
      | t -> zadd (ZERO::b) (t-1)
    )
  in
  let rec zrmv : bin -> bin = fun b -> 
    (match b with
      | [] -> []
      | hd::tl -> 
        (match hd with
          | ZERO -> zrmv tl
          | _ -> b
        )
    )
  in
  let rec bflip : bin -> bin = fun b ->
    (match b with
     | [] -> []
     | hd::tl -> (bflip tl) @ [hd]
    )
  in
  let rec fadd : bin -> bin -> bin -> bin = fun b1 b2 carry ->
    (match (b1, b2, carry) with
    |([], [], c) -> c
    |(hd1::tl1, hd2::tl2, c::_) ->
      (match (hd1, hd2, c) with
       | (ZERO, ZERO, ZERO) 
          -> (fadd tl1 tl2 [ZERO]) @ [ZERO]
       | (ZERO, ZERO, ONE) | (ZERO, ONE, ZERO) | (ONE, ZERO, ZERO) 
          -> (fadd tl1 tl2 [ZERO]) @ [ONE]
       | (ONE, ONE, ZERO) | (ONE, ZERO, ONE) | (ZERO, ONE, ONE)  
          -> (fadd tl1 tl2 [ONE]) @ [ZERO]
       | (ONE, ONE, ONE) 
          -> (fadd tl1 tl2 [ONE]) @ [ONE]
      )
    )
  in
  let badd : bin -> bin -> bin = fun b1 b2 ->
    let l1 = blen b1 in let l2 = blen b2 in
    (match if l1 > l2 then (b1, zadd b2 (l1 - l2)) else (zadd b1 (l2 - l1), b2) with
    | (bin1, bin2) -> fadd (bflip bin1) (bflip bin2) [ZERO]
    )
  in
  let result = zrmv (match (b1, b2) with
  | ([ZERO], _) | (_, [ZERO]) -> [ZERO]
  | ([ONE], b) | (b, [ONE] ) -> b
  | (hd::tl, b) -> badd ((bmul [hd] b) @ (zadd [] (blen tl))) (bmul tl b)
  )
  in
  match result with
  | [] -> [ZERO]
  | _ -> result
  
