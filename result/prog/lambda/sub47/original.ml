type lambda = V of var | P of (var * lambda) | C of (lambda * lambda)

and var = string

let rec check (m : lambda) : bool =
  match m with
  | V _ -> false
  | C _ -> false
  | P (n', m') -> List.mem n' (listMetro m')


and listMetro (m : lambda) : 'a list =
  match m with
  | V m' -> [ m' ]
  | C (m1, m2) -> (
      match (listMetro m1, listMetro m2) with
      | a :: b, c :: d -> List.append (a :: b) (c :: d)
      | _, _ -> [] )
  | P (n', m') -> ( match check m with true -> listMetro m' | false -> [] )
