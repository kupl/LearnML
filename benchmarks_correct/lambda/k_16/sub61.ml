
  type lambda =
  | V of var
  | P of var * lambda
  | C of lambda * lambda
  and var = string

  let check : lambda -> bool
  = fun lambda ->
      let rec contains v vlist = match vlist with
        | h :: t -> if v = h then true else contains v t
        | _ -> false in
      let rec chk e vlist = match e with
        | P (v, ne) -> chk ne (v :: vlist)
        | C (e1, e2) -> (chk e1 vlist) && (chk e2 vlist)
        | V v -> contains v vlist
      in chk lambda []
