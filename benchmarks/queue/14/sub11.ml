module type Queue =
  sig (* ��� ���� ���� *)
    type element
    type queue
    exception EMPTY_Q
    val emptyQ: queue
    val enQ: queue * element -> queue
    val deQ: queue -> element * queue
  end

let rev list =
    let rec aux add = function
      | [] -> add
      | h::t -> aux (h::add) t in
    aux [] list;;

let length list =
    let rec aux n = function
      | [] -> n
      | _::t -> aux (n+1) t
    in aux 0 list;;

module IntListQ =
  struct
    type element = int list
    type queue = (element list * element list) (* �ֱ� *)
    exception EMPTY_Q
    let emptyQ = ([], []) (* �ֱ� *)
    let enQ arg = (* ���� *)
      let f1 = fst(arg) in
        let e1 = snd(arg) in
          let f2 = fst(f1) in
            let e2 = snd(f1) in
              if (length f2 <= length e2)
                then (e1::f2, e2)
              else
                (e1::[], List.append e2 (rev f2))

    let rec deQ = function (*���� *)
        ([], []) -> raise EMPTY_Q
      | (a,  []) -> deQ ([], (rev a))
      | (a,  b::bt) -> (b, (a, bt))

end
