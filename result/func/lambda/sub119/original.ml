type lambda = V of var | P of (var * lambda) | C of (lambda * lambda)

and var = string

let rec check (lam : lambda) : bool =
  let rec eval (lam : lambda) (ll : string list) : var list =
    match lam with
    | V v -> (
        match ll with
        | [] -> "false" :: ll
        | hd :: tl -> if hd = v then ll else hd :: eval (V v) tl )
    | P (v, l) ->
        let ll2 : string list = v :: ll in
        eval l ll2
    | C (l1, l2) -> eval l2 (eval l1 ll)
  in

  let rec compare (ll : string list) : bool =
    match ll with
    | [] -> true
    | hd :: tl -> if hd = "false" then false else compare tl
  in
  compare (eval lam [])
