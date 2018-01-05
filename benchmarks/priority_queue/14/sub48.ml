type heap = EMPTY | NODE of rank * value * heap * heap
and rank = int
and value = int

exception EmptyHeap
let rank: heap -> int =
    fun h ->
        match h with
        | EMPTY -> -1
        | NODE(r, v, h1, h2) -> r

let shake: value * heap * heap -> heap =
    fun (v, lh, rh) ->
        if (rank lh) >= (rank rh)
            then NODE((rank rh)+1, v, lh, rh)
            else NODE((rank lh)+1, v, rh, lh)

let findMin: heap -> value =
    fun h ->
        match h with
        | EMPTY -> raise EmptyHeap
        | NODE(_, x, _, _) -> x

let getRight: heap -> heap =
    fun h ->
        match h with
        | EMPTY -> raise EmptyHeap
        | NODE(r, v, h1, h2) -> h2

let getLeft: heap -> heap =
    fun h ->
        match h with
        | EMPTY -> raise EmptyHeap
        | NODE(r, v, h1, h2) -> h1

let rec merge: heap * heap -> heap =
    fun (h1, h2) ->
        match h1, h2 with
        | EMPTY, _ -> h2
        | _, EMPTY -> h1
        | _, _ -> if (findMin h1)>(findMin h2) then shake((findMin h2), (getLeft h2), merge((getRight h2), h1))
                else shake((findMin h1), (getLeft h1), merge((getRight h1), h2))

let insert: value * heap -> heap =
    fun (v, h) ->
        merge(h, NODE(0, v, EMPTY, EMPTY))

let deleteMin: heap -> heap =
    fun h ->
        match h with
        | EMPTY -> raise EmptyHeap
        | NODE(_, x, lh, rh) -> merge(lh, rh)
