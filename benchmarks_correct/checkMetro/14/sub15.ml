(*
 * 컴퓨터공학부 2009-11690 김찬민
 * Homework 2 Exercise 1  *)

type metro = STATION of name
           | AREA of name * metro
           | CONNECT of metro * metro
and name = string

let checkMetro (wholeMap : metro) : bool =
    let rec checkSubMetro curMap areaStack =
        match curMap with
        | STATION(stationName) ->
            List.exists (fun areaName -> areaName = stationName) areaStack
        | AREA(areaName, subMap) -> checkSubMetro subMap (areaName::areaStack)
        | CONNECT(sub1, sub2) ->
            (checkSubMetro sub1 areaStack) &&
            (checkSubMetro sub2 areaStack)
    in
    checkSubMetro wholeMap []
