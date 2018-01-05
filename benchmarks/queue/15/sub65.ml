(*2-6 컴공 2014-10618 이세영*)
module type Queue=
    sig
        type element
        type queue
        exception EMPTY_Q
        val emptyQ: queue
        val enQ: queue*element->queue
        val deQ: queue->element*queue
    end
module IntListQ =
    struct
        type element = int list
        type queue = element list* element list
        exception EMPTY_Q
        let emptyQ:queue = ([],[])
        let enQ (q, e)=
            match q with
           (f,r)->(e::f,r)
        let rec deQ q=
            match q with
            |([],[])->raise EMPTY_Q
            |(f,[])->deQ ([],(List.rev f))
            |(f,e::r)->(e,(f,r))
    end;;
