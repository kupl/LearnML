
  type lambda =
  | V of var
  | P of var * lambda
  | C of lambda * lambda
  and var = string

  let rec evaluate temp1 temp2 =
    match temp1 with
    V a ->
        let rec ck l1 st =
          match l1 with
          [] -> false |
          h::t -> if h=st then true else (ck t st) in
        if (ck temp2 a) then true else false |
    P (v, e1) -> if (evaluate e1 (temp2@[v])) then true else false |
    C (e1, e2) -> if (evaluate e1 temp2 && evaluate e2 temp2) then true else false;;

let check : lambda -> bool
=fun lambda ->
      evaluate lambda []
