type lambda = V of var | P of (var * lambda) | C of (lambda * lambda)

and var = string

let rec __s3 (__s4 : lambda) : string list =
  match __s4 with
  | V __s5 -> [ __s5 ]
  | P (__s6, __s7) ->
      List.filter (fun (__s8 : string) -> not (__s6 = __s8)) (__s3 __s7)
  | C (__s9, __s10) -> __s3 __s9 @ __s3 __s10


let rec check (e : lambda) : bool =
  let rec checkp ((va : string), (ex : lambda)) : bool =
    match ex with
    | V va2 -> if va = va2 then true else false
    | P (va2, ex2) -> (
        match ex2 with
        | C (ex3, ex4) ->
            if
              (checkp (va, ex3) || checkp (va2, ex3))
              && (checkp (va, ex4) || checkp (va2, ex4))
            then true
            else false
        | _ -> if checkp (va, ex2) || checkp (va2, ex2) then true else false )
    | C (ex2, ex3) ->
        if checkp (va, ex2) && checkp (va, ex3) then true else false
  in
  List.length (__s3 e) = 0
