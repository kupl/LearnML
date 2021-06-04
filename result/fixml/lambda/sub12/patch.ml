exception Invalid_input of string

type lambda = V of var | P of (var * lambda) | C of (lambda * lambda)

and var = string

let rec check m =
  let rec check_sub (m, l) =
    let rec checkArea (s, l) =
      match l with
      | [] -> false
      | h :: t -> if h = s then true else checkArea (s, t)
    in

    match m with
    | V s -> checkArea (s, l)
    | P (a, m) -> check_sub (m, a :: l)
    | C (m0, m1) -> check_sub (m0, l) && check_sub (m1, l)
  in

  match m with V s -> false | _ -> check_sub (m, [])
