module IntListQ =
struct
type element = int list
type queue = element*element
exception EMPTY_Q
let emptyQ q=
let enQ (q,e)= (match q with
        (e1,e2)->(e1@e,e2)
        )
let deQ q= ...
end
let rec flip_list l=
        let rec list_cutend l=(match l with
        [x]->[]
        |_->List.hd(l)::(list_cutend (List.tl(l)))
        ) in
        (match l with
        []->l
        |[x]->l
        |_->[List.nth l (List.length(l)-1)]@(flip_list((list_cutend
        (List.tl l))))@[List.hd l]
        )
