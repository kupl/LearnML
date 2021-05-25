type lambda = V of var
           | P of var * lambda
           | C of lambda * lambda
and var = string

let check : lambda -> bool =
  fun a ->
    let rec checkM (a, lst) =
      match a with
      | V id -> List.exists (fun b -> id = b) lst
      | P (id, met) -> checkM (met, id::lst)
      | C (met1, met2) -> (checkM (met1, lst)) && (checkM (met2, lst)) in
    (checkM (a, []))
