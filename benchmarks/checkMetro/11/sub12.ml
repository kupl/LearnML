(* PL HW1-7 "Check Metro Map"
   2007-11738
   알렉산더 *)

type metro = STATION of name
           | AREA of name * metro
           | CONNECT of metro * metro

and name = string

(* checkMetro: metro -> bool *)
let checkMetro mtr =
    (* subfunction with area names list *)
    let rec checkMetroInner (met, aNameList) =
        match met with
             STATION stName -> if (List.mem stName aNameList) then true
                               else false
            |CONNECT (a, b) -> ((checkMetroInner (a, aNameList)) && (checkMetroInner (b, aNameList)))
            |AREA (aName, CONNECT (a, b)) -> checkMetroInner (CONNECT (AREA (aName, a),AREA (aName, b)),
                                                              aNameList)
            |AREA (aName, m) -> checkMetroInner (m, aName::aNameList)
    in

    checkMetroInner (mtr, [])

(*
    aName - AREA name
    stName - STATION name
    aNameList - list of AREA names
*)
