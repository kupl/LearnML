type lambda = V of var | P of (var * lambda) | C of (lambda * lambda)

and var = string

let bindlst : 'a list = []

let rec check (lam : lambda) : bool =
  let rec bindchk (sublam : lambda) (benv : string list) : var list =
    match sublam with
    | V x -> benv
    | P (x, e) -> bindchk e (x :: benv)
    | _ -> benv
  in

  let bindlst : string list = bindchk lam [] in

  let lookup (x : string) (blst : string list) : bool =
    let rec lookupproc (xx : string) (lst : string list) : bool =
      match lst with
      | [] -> false
      | hd :: tl -> if hd = xx then true else lookupproc xx tl
    in
    lookupproc x blst
  in

  let rec isbind (lda : lambda) (bindenv : string list) : bool =
    match lda with
    | V x -> lookup x bindenv
    | P (x, e) -> isbind e (x :: bindenv)
    | C (e1, e2) ->
        if isbind e1 bindenv = true && isbind e2 bindenv = true then true
        else false
  in
  isbind lam bindlst
