type lambda = V of var | P of (var * lambda) | C of (lambda * lambda)

and var = string

let bindlst = []

let rec check : lambda -> bool =
 fun lam ->
  let rec bindchk : lambda -> var list -> var list =
   fun sublam benv ->
    match sublam with
    | V x -> benv
    | P (x, e) -> bindchk e (x :: benv)
    | C (e1, e2) -> bindchk e2 (bindchk e1 benv)
  in

  let bindlst = bindchk lam [] in

  let lookup : var -> var list -> bool =
   fun x blst ->
    let rec lookupproc : var -> var list -> bool =
     fun xx lst ->
      match lst with
      | [] -> false
      | hd :: tl -> if hd = xx then true else lookupproc xx tl
    in
    lookupproc x blst
  in

  let rec isbind : lambda -> var list -> bool =
   fun lda bindenv ->
    match lda with
    | V x -> lookup x bindenv
    | P (x, e) -> isbind e bindenv
    | C (e1, e2) ->
        if isbind e1 bindenv = true && isbind e2 bindenv = true then true
        else false
  in
  isbind lam bindlst
