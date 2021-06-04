type lambda = V of var | P of (var * lambda) | C of (lambda * lambda)

and var = string

let check (m : lambda) : bool =
  let rec idCheck (id_list : string list) (m : lambda) : bool =
    let rec exists (f : string) (l : string list) : bool =
      match l with [] -> false | h :: t -> if f = h then true else exists f t
    in

    match m with
    | V a -> exists a id_list
    | P (id1, m1) -> idCheck (id1 :: id_list) m1
    | C (m1, m2) -> idCheck id_list m1 && idCheck id_list m2
  in
  idCheck [] m
