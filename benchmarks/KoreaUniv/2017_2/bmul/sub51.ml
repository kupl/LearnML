type digit = ZERO | ONE
type bin = digit list

let digadd : digit -> digit -> digit -> digit * digit
= fun b1 b2 rise ->
  match (b1, b2, rise) with
  | (ZERO, ZERO, ZERO) -> (ZERO, ZERO)
  | (ZERO, ZERO, ONE) -> (ONE, ZERO)
  | (ZERO, ONE, ZERO) -> (ONE, ZERO)
  | (ONE, ZERO, ZERO) -> (ONE, ZERO)
  | (ONE, ONE, ONE) -> (ONE, ONE)
  | _ -> (ZERO, ONE)

let badd : bin -> bin -> bin
= fun b1 b2 ->
  let rec reversebin : bin -> bin
  = fun b ->
    match b with
    | [] -> []
    | h :: t -> (reversebin t) @ [h]
  in
    let rec adding : bin -> bin -> digit -> bin
    = fun bi1 bi2 rise ->
      match (reversebin bi1, reversebin bi2) with
      | ([], []) -> if rise = ZERO then [] else [ONE]
      | (h :: t, []) -> 
        (match (digadd h ZERO rise) with
        | (con, ris) -> (adding (reversebin t) [] ris) @ [con])
      | ([],h :: t) -> 
        (match (digadd ZERO h rise) with
        | (con, ris) -> (adding [] (reversebin t) ris) @ [con])
      | (h1::t1, h2::t2) -> 
        (match (digadd h1 h2 rise) with
        | (con, ris) -> (adding (reversebin t1) (reversebin t2) ris) @ [con])
    in
      adding b1 b2 ZERO;;

let rec bmul : bin -> bin -> bin
= fun b1 b2 ->
  let rec reversebin : bin -> bin
  = fun b ->
    match b with 
    | [] -> []
    | h :: t ->  (reversebin t) @ [h]
  in
  match (reversebin b1) with
  | [] -> [ZERO]
  | [ZERO] -> [ZERO]
  | h1 :: t1 -> 
    match (reversebin b2) with
    | [] -> [ZERO]
    | [ZERO] -> [ZERO]
    | h2 :: t2 -> 
      match h2 with
      | ONE -> badd b1 ((bmul b1 (reversebin t2)) @ [ZERO]) 
      | ZERO -> (bmul b1 (reversebin t2)) @ [ZERO];;
