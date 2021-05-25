

(* Author: Arif Jafer, 2012-11255 *)
(* PL, Spring 2014 *)

(* HW2-Q1: checkMap *)

type lambda =
        | V of var
        | P of var * lambda
        | C of lambda * lambda

and var = string ;;

let rec is_member y lst =
        match lst with
        | [] -> false
        | x :: xl -> (x = y) || (is_member y xl);;

let check = fun m ->
        let rec checkRec lambda accm =
                match lambda with
                | V id -> is_member id accm
                | P (id, m1) -> checkRec m1 (id :: accm)
                | C (m1, m2) ->   (checkRec m1 accm) &&
                                        (checkRec m2 accm)
         in

         checkRec m [];;



