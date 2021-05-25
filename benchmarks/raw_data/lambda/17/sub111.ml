(*
    Homework 2, Exercise 4
    2015-15894 Jonghoon Won
    Sep 28, 2017
*)

type lambda = V of var
           | P of var * lambda
           | C of lambda * lambda
and var = string

let rec check: lambda -> bool =
    let rec checkInList: 'a * 'a list -> bool = fun (value, lst) ->
        match lst with
        | [] -> false
        | hd::tl -> if value = hd then true else checkInList (value, tl)
    in

    let rec checkHelper: string list * lambda -> bool = fun (areaList, met) ->
        match met with
        | V (stationName) -> checkInList (stationName, areaList)
        | P (areaName, nextMetro) -> (
            if (checkInList (areaName, areaList)) then checkHelper (areaList, nextMetro)
            else checkHelper (areaName::areaList, nextMetro)
            )
        | C (lambda1, lambda2) ->
            checkHelper (areaList, lambda1) && checkHelper (areaList, lambda2)
    in

    fun met -> checkHelper ([], met)
