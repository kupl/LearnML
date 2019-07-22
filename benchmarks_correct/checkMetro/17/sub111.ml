(*
    Homework 2, Exercise 4
    2015-15894 Jonghoon Won
    Sep 28, 2017
*)

type metro = STATION of name
           | AREA of name * metro
           | CONNECT of metro * metro
and name = string

let rec checkMetro: metro -> bool =
    let rec checkInList: 'a * 'a list -> bool = fun (value, lst) ->
        match lst with
        | [] -> false
        | hd::tl -> if value = hd then true else checkInList (value, tl)
    in

    let rec checkMetroHelper: string list * metro -> bool = fun (areaList, met) ->
        match met with
        | STATION (stationName) -> checkInList (stationName, areaList)
        | AREA (areaName, nextMetro) -> (
            if (checkInList (areaName, areaList)) then checkMetroHelper (areaList, nextMetro)
            else checkMetroHelper (areaName::areaList, nextMetro)
            )
        | CONNECT (metro1, metro2) ->
            checkMetroHelper (areaList, metro1) && checkMetroHelper (areaList, metro2)
    in

    fun met -> checkMetroHelper ([], met)
