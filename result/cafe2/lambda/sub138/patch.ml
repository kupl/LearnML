type lambda = V of var | P of (var * lambda) | C of (lambda * lambda)

and var = string

let rec __s3 (__s4 : lambda) : string list =
  match __s4 with
  | V __s5 -> [ __s5 ]
  | P (__s6, __s7) ->
      List.filter (fun (__s8 : string) -> not (__s6 = __s8)) (__s3 __s7)
  | C (__s9, __s10) -> __s3 __s9 @ __s3 __s10


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
  List.length (__s3 lam) = 0
