type lambda = V of var | P of (var * lambda) | C of (lambda * lambda)

and var = string

let check (lam : lambda) : bool =
  let rec check1 (lambda : lambda) (myvarlst : string list) : string list =
    match lambda with
    | V v -> myvarlst
    | P (v, l) -> check1 l (v :: myvarlst)
    | C (l1, l2) -> check1 l1 myvarlst @ check1 l1 []
  in

  let rec check2 (lambda : lambda) (myfreevarlst : string list) : string list =
    match lambda with
    | V v -> v :: myfreevarlst
    | P (v, l) -> check2 l myfreevarlst
    | C (l1, l2) -> check2 l1 myfreevarlst @ check2 l2 []
  in

  let rec check3 (varlst : string list) (freevarlst : string list) : bool =
    match freevarlst with
    | [] -> true
    | var :: tl -> (
        match varlst with
        | [] -> false
        | var2 :: tl2 ->
            if var = var2 then check3 tl freevarlst else check3 varlst tl2 )
  in

  let varlst : string list = check1 lam []
  and freevarlst : string list = check2 lam [] in
  check3 varlst freevarlst
