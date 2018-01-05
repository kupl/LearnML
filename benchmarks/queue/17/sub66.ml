module type Queue =
  sig
    type element
    type queue
    exception EMPTY_Q
    val emptyQ : queue
    val enQ : queue * element -> queue
    val deQ : queue -> element * queue
  end

module IntListQ =
  struct
    type element = int list
    type queue = Q of element list * element list
    exception EMPTY_Q
    let emptyQ : queue = Q([], [])
    let enQ (q, e) =
      match q with Q (en, de) -> Q(e::en, de)

    let deQ q =
      match q with
      | Q ([], []) -> raise EMPTY_Q
      | Q (en, []) ->
        (let rec shift l =
          match l with
          | Q([], b) -> Q([], b)
          | Q(h::t, b) -> shift (Q(t, h::b))
        in
        match (shift q) with
        | Q(en, dh::dt) -> dh, (Q(en, dt))
        | Q(en, _) -> raise EMPTY_Q)
      | Q (en, dh::dt) -> dh, (Q(en, dt))
  end
