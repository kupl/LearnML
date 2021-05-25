(* #HW6- nat *)
type nat = 
  | ZERO
  | SUCC of nat;;
let netadd a1 a2=
  let rec net_to_number (n:nat) : int =
    match n with
      | ZERO -> 0
      | SUCC m -> 1+ net_to_number m in
  let rec number_to_net n =
    match n with
      | 0 -> ZERO
      | _ -> if n < 0 then failwith "not minus" else SUCC (number_to_net (n-1)) in
  number_to_net ((net_to_number a1) + (net_to_number a2));;
let netmul a1 a2 =
  let rec net_to_number (n:nat) : int =
    match n with
      | ZERO -> 0
      | SUCC m -> 1+ net_to_number m in
  let rec number_to_net n =
    match n with
      | 0 -> ZERO
      | _ -> if n < 0 then failwith "not minus" else SUCC (number_to_net (n-1)) in
  number_to_net ((net_to_number a1) * (net_to_number a2));;