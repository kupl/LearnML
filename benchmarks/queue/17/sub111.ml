(*
    Homework 2, Exercise 6
    2015-15894 Jonghoon Won
    Sep 28, 2017
*)

module type Queue = sig
    type element
    type queue
    exception EMPTY_Q
    val emptyQ: queue
    val enQ: queue * element -> queue
    val deQ: queue -> element * queue
end

module IntListQ = struct
    type element = int list
    type queue = element list * element list
    exception EMPTY_Q
    let emptyQ: queue = ([], [])

    let enQ: queue * element -> queue = fun (q, elem) ->
        match q with (lst1, lst2) -> (elem::lst1, lst2)

    let deQ: queue -> element * queue = fun q ->
        let rec pushToRight: queue -> queue = fun q ->
            match q with
                  ([], lst2) -> ([], lst2)
                | (hd::tl, lst2) -> pushToRight (tl, hd::lst2)
        in

        match q with
              ([], []) -> raise EMPTY_Q
            | (lst1, []) -> (
                match (pushToRight q) with
                      (_, []) -> raise EMPTY_Q (* this case never happens: added only for exhaustiveness *)
                    | (_, hd::tl) -> (hd, ([], tl))
                )
            | (lst1, hd::tl) -> (hd, (lst1, tl))
end
