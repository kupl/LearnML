(* PL HW1-7 "Check Metro Map"
   2007-11738
   알렉산더 *)

type lambda = V of var
           | P of var * lambda
           | C of lambda * lambda

and var = string

(* check: lambda -> bool *)
let check mtr =
    (* subfunction with area vars list *)
    let rec checkInner (met, aNameList) =
        match met with
             V stName -> if (List.mem stName aNameList) then true
                               else false
            |C (a, b) -> ((checkInner (a, aNameList)) && (checkInner (b, aNameList)))
            |P (aName, C (a, b)) -> checkInner (C (P (aName, a),P (aName, b)),
                                                              aNameList)
            |P (aName, m) -> checkInner (m, aName::aNameList)
    in

    checkInner (mtr, [])

(*
    aName - P var
    stName - V var
    aNameList - list of P vars
*)
