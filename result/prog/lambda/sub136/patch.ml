type lambda = V of var | P of (var * lambda) | C of (lambda * lambda)

and var = string

let bindlst : 'a list = []

let rec __s3 ((__s4 : lambda), (__s5 : string list)) :
    (lambda * string list) list =
  match (__s4, __s5) with
  | P (__s16, __s17), __s18 -> __s3 (__s17, __s16 :: __s18)
  | V __s19, __s20 -> [ (V __s19, __s20) ]
  | C (__s21, __s22), __s23 ->
      List.append (__s3 (__s21, __s23)) (__s3 (__s22, __s23))


let __s6 (__s7 : lambda * string list) : bool =
  match __s7 with
  | V __s8, __s9 -> List.mem __s8 __s9
  | P (__s10, __s11), __s12 -> false
  | C (__s13, __s14), __s15 -> false


let rec check (lam : lambda) : bool =
  let rec bindchk (sublam : lambda) (benv : string list) : var list =
    match sublam with
    | V x -> benv
    | P (x, e) -> bindchk e (x :: benv)
    | C (e1, e2) -> bindchk e2 (bindchk e1 benv)
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
    | P (x, e) -> isbind e bindenv
    | C (e1, e2) ->
        if isbind e1 bindenv = true && isbind e2 bindenv = true then true
        else false
  in
  List.for_all __s6 (__s3 (lam, []))
