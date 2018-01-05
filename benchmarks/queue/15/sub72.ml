module type Queue =
    sig
        type element
        type queue
        exception EMPTY_Q
        val emptyQ: queue
        val enQ: queue * element -> queue
        val deQ: queue -> element * queue
    end;;

module IntListQ =
    struct
        type element = int list
        type queue = int list list * int list list
        exception EMPTY_Q
        let emptyQ = ([], [])
        let enQ (queue, el) = match queue with (left, right) -> (el::left, right)
        let rec deQ queue =
            let reverseLeft queue = 
                match queue with
                |(left, []) -> ([], List.rev left)
                |(_, _) -> queue
            in
            match queue with
            |([], []) -> raise EMPTY_Q
            |(left, []) -> deQ (reverseLeft queue)
            |(left, a::right) -> (a, (left, right))
    end;;

  let q1 = IntListQ.emptyQ 
  let q2 = IntListQ.enQ(IntListQ.enQ(IntListQ.enQ(q1, [1]), [2; 3]), [4; 5; 6]) 
  let (e1, q3) = IntListQ.deQ q2    ;; 
