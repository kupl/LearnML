type lambda = V of var | P of (var * lambda) | C of (lambda * lambda)

and var = string

let isStringExist (e : string) (l : 'a list) : bool =
  let x = List.find (fun x -> x = e) l in
  true


let rec __s1 __s2 (__s3 : 'b list) : bool =
  match __s3 with
  | [] -> false
  | __s15 :: __s16 -> if __s15 = __s2 then true else __s1 __s2 __s16


let rec check (lambda : lambda) : bool =
  let rec checkWithArea (areaList : string list) (lambda : lambda) : bool =
    match lambda with
    | V var -> __s1 var areaList
    | P (var, lambda) -> checkWithArea (areaList @ [ var ]) lambda
    | C (m1, m2) -> checkWithArea areaList m1 && checkWithArea areaList m2
  in
  checkWithArea [] lambda
