(* #HW 2- prime *)
let prime a =
  let rec prime_test a1 a2 =
    if a1=a2 then true else ((a1 mod a2) <> 0) && prime_test a1 (a2+1)
  in
  match a with
    | 0 -> false
    | 1 -> false
    | 2 -> true
    | _ -> prime_test a 2;;