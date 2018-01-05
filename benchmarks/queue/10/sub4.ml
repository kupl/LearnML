(* 컴퓨터공학부 / 2005-11721 / 김재경 / 숙제2-4 *)
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
    type queue = element * element
    exception EMPTY_Q
    let emptyQ = ([],[])
    let enQ(q, l) =
      match q with
        (left, right) -> (l @ left, right)
    let get_first(list) =
      match list with
        first :: list_ex -> [first]
      | _ -> []
    let get_extra(list) =
      match list with
        first :: list_ex -> list_ex
      | _ -> []                        
    let deQ(q) =
      match q with
        (left, right) ->
          let rec deQ_fun(left, right) =
            if right != [] then
              (get_first(right), (left, get_extra(right)))
            else if left = [] then raise(EMPTY_Q)
            else
              let rec left_to_right(left1, right1) =
                match left1 with
                  a::left_ex -> left_to_right(left_ex, a::right)
                | [] -> (left1, right1) in
              deQ_fun(left_to_right(left, right))
          in
          deQ_fun(left, right)
  end