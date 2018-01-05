(* 2014-18790 JangHo Seo <jangho.se@snu.ac.kr>
 * Programming Languages 2015 Fall
 * Homework 2, Exercise 3 *)

type metro = STATION of name
           | AREA of name * metro
           | CONNECT of metro * metro
and name = string

(* helper function isStringExist *)
let isStringExist e l =
    try
        let _ = List.find (fun x -> (String.compare x e == 0)) l in
        true
    with Not_found -> false

(* checkMetro *)
let checkMetro metro =
    let rec checkMetroWithArea areaList metro =
        match metro with
        | STATION name -> isStringExist name areaList
        | AREA (name, metro) -> checkMetroWithArea (areaList@[name]) metro
        | CONNECT (m1, m2) -> (checkMetroWithArea areaList m1) &&
                              (checkMetroWithArea areaList m2)
    in
    checkMetroWithArea [] metro
