(* 2014-18790 JangHo Seo <jangho.se@snu.ac.kr>
 * Programming Languages 2015 Fall
 * Homework 1, Exercise 1 *)

(* do we have finished scanning this list? *)
let list_finished lst pos = List.length lst <= pos

(* from what list we should choose the next element *)
type from = A | B | None

(* recursive definition of grow *)
let rec grow a a_pos b b_pos result =
    let a_finished = list_finished a a_pos in
    let b_finished = list_finished b b_pos in

    (* from what list we should choose the next element? *)
    let what : from =
        if a_finished then
            if b_finished then
                (* nothing to do *)
                None
            else
                (* only list b has things left *)
                B
        else
            if b_finished then
                (* only list a has things left *)
                A
            else
                (* which one is bigger? *)
                if List.nth a a_pos > List.nth b b_pos then
                    A
                else
                    B
    in

    (* grow! *)
    match what with
    |A -> grow a (a_pos + 1) b b_pos (result@[List.nth a a_pos])
    |B -> grow a a_pos b (b_pos + 1) (result@[List.nth b b_pos])
    |None -> result

(* merge: int list * int list -> int list *)
let merge (a, b) = grow a 0 b 0 []
