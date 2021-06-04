exception Invalid_input of string

type lambda = V of var | P of (var * lambda) | C of (lambda * lambda)

and var = string

let rec check (m : lambda) : bool =
  let rec check_sub ((m : lambda), (l : string list)) : bool =
    let rec checkArea ((s : string), (l : string list)) : bool =
      match l with
      | [] -> false
      | h :: t -> if h = s then true else checkArea (s, t)
    in

    match m with
    | V s -> checkArea (s, l)
    | P (a, m) -> check_sub (m, a :: l)
    | C (m0, m1) -> check_sub (m0, l) && check_sub (m1, l)
  in
  check_sub (m, [])
