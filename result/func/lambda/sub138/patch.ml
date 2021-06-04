type lambda = V of var | P of (var * lambda) | C of (lambda * lambda)

and var = string

let rec __s1 (__s2 : lambda) (__s3 : string list) : bool =
  match __s2 with
  | V __s8 -> (
      match __s3 with
      | __s9 :: __s10 -> if __s9 = __s8 then true else __s1 __s2 __s10
      | [] -> false )
  | P (__s11, __s12) -> __s1 __s12 (__s11 :: __s3)
  | C (__s13, __s14) -> __s1 __s13 __s3 && __s1 __s14 __s3


let check (lam : lambda) : bool =
  let myvarlst : string list = [] and myfreevarlst : string list = [] in

  let rec check2 (lambda : lambda) : string list =
    match lambda with
    | V v -> myfreevarlst @ [ v ]
    | P (v, l) ->
        let dummy1 : string list = myvarlst @ [ v ] in
        check2 l
    | C (l1, l2) ->
        let dummy2 : string list = check2 l1 in
        check2 l2
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

  let dummy : string list = check2 lam in

  match lam with V __s7 -> false | _ -> __s1 lam myvarlst
