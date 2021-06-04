type lambda = V of var | P of (var * lambda) | C of (lambda * lambda)

and var = string

let rec check m =
  let rec append m1 m2 =
    match m1 with
    | h :: t -> if List.mem h m2 then append t m2 else append t (h :: m2)
    | [] -> m2
  in

  let rec check2 m l =
    match m with
    | V n -> l
    | P (n, m2) -> if List.mem n l then check2 m2 l else check2 m2 (n :: l)
    | C (m1, m2) -> append (check2 m1 l) (check2 m2 l)
  in

  let rec check3 m l =
    match m with
    | V n -> if List.mem n l then true else false
    | P (n, m2) -> check3 m2 l
    | C (m1, m2) -> check3 m1 l && check3 m2 l
  in

  let l = check2 m [] in
  check3 m l
