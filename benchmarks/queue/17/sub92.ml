(* HW2 Exercise 6 Queue *)

module type Queue =
sig
    type element
    type queue
    exception EMPTY_Q
    val emptyQ: queue
    val enQ: queue * element -> queue
    val deQ: queue -> element * queue
end

module IntListQ =
struct
  type element = int list
  type queue = element list * element list
  exception EMPTY_Q
  let emptyQ: queue = ([], [])
  let enQ: queue * element -> queue = fun (q, e) ->
    match q with
    | (list1, list2) -> (e::list1, list2)
  let deQ: queue -> element * queue = fun q ->
    match q with
    | ([], []) -> raise (EMPTY_Q)
    | (list1, []) -> (
      let rec reverse_append: element list * element list -> element list * element list = fun (list1, list2) ->
        match list1 with
        | [] -> ([], list2)
        | head::tail -> reverse_append (tail, head::list2)
      in

      let (new_list1, new_list2) = reverse_append (list1, []) in
      match new_list2 with
      | [] -> raise (EMPTY_Q)
      | head::tail -> (head, (new_list1, tail))
      )
    | (list1, head::tail) -> (head, (list1, tail))
end
