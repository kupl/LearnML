type lambda = V of var | P of (var * lambda) | C of (lambda * lambda)

and var = string

let rec recheck (v : string list) (lamm : lambda) : bool =
  match lamm with
  | P (x, l) -> recheck (v @ [ x ]) l
  | C (l1, l2) -> recheck v l1 && recheck v l2
  | V x -> (
      match v with
      | [] -> false
      | hd :: tl -> if hd = x then true else recheck tl lamm )


let rec check (lam : lambda) : bool =
  match lam with
  | P (v, l) -> recheck [ v ] l
  | C (__s12, __s13) -> check __s13 && check __s12
  | _ -> false
