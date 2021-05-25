(*
 * 컴퓨터공학부 2009-11690 김찬민
 * Homework 2 Exercise 1  *)

type lambda = V of var
           | P of var * lambda
           | C of lambda * lambda
and var = string

let check (wholeMap : lambda) : bool =
    let rec checkSubMetro curMap areaStack =
        match curMap with
        | V(stationName) ->
            List.exists (fun areaName -> areaName = stationName) areaStack
        | P(areaName, subMap) -> checkSubMetro subMap (areaName::areaStack)
        | C(sub1, sub2) ->
            (checkSubMetro sub1 areaStack) &&
            (checkSubMetro sub2 areaStack)
    in
    checkSubMetro wholeMap []
