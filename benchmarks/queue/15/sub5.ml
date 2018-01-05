(* SNU Programming Language Fall 2015
 * Homework 2 
 * Exercise 6: IntListQ
 * Written by Dongho Kang 
 *)

module type Queue =
sig
    type element
    type queue
    exception EMPTY_Q
    val emptyQ: queue
    val enQ: queue * element -> queue 
    val deQ: queue -> element * queue
end
;;

module IntListQ =
struct
    type element    = int list 
    type queue      = element list * element list
    exception EMPTY_Q

    let emptyQ : queue = ([], [])

    let enQ (q, e) : queue = 
        match q with 
        | (l_list, r_list) -> (e :: l_list, r_list) (* enQ: add element in left list *)

    let deQ (q : queue) : (element * queue) =       (* deQ: extract element from right list *)
        match q with 
        | ([], [])          -> raise EMPTY_Q    (* exception: cannot be deQ when Q is empty*)
        | (l_list, [])      ->                  (* recursive case1: when right list is empty. *)
                let l_list_rev = List.rev l_list in
                (List.hd l_list_rev , ([], List.tl l_list_rev))
        | (l_list, r_list)  -> (List.hd r_list, (l_list, List.tl r_list)) (* recursive case2: usual case *)
end
;;
