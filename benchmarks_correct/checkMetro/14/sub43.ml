

(* Author: Arif Jafer, 2012-11255 *)
(* PL, Spring 2014 *)

(* HW2-Q1: checkMetroMap *)

type metro =
        | STATION of name
        | AREA of name * metro
        | CONNECT of metro * metro

and name = string ;;

let rec is_member y lst =
        match lst with
        | [] -> false
        | x :: xl -> (x = y) || (is_member y xl);;

let checkMetro = fun m ->
        let rec checkMetroRec metro accm =
                match metro with
                | STATION id -> is_member id accm
                | AREA (id, m1) -> checkMetroRec m1 (id :: accm)
                | CONNECT (m1, m2) ->   (checkMetroRec m1 accm) &&
                                        (checkMetroRec m2 accm)
         in

         checkMetroRec m [];;



