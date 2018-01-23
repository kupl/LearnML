type heap = EMPTY
| NODE of rank * value * heap * heap
and rank = int
and value = int

exception EmptyHeap
let rank =
    (
        function 
            EMPTY -> -1
        |   NODE(r, _, _, _) -> r
    )
let findMin =
    (
        function
            EMPTY -> raise EmptyHeap
        |   NODE(_, x, _, _) -> x
    )
let rec insert =
    (
        function (x, h) -> merge(h, NODE(0, x, EMPTY, EMPTY))
    )
and deleteMin =
    (
        function
            EMPTY -> raise EmptyHeap
        |   NODE(_, x, lh, rh) -> merge(lh, rh)
    )
and merge =
    (
        let shake =
            (
                function (x, lh, rh) ->
                    if ((rank lh) >= (rank rh))
                    then NODE((rank rh) + 1, x, lh, rh)
                    else NODE((rank lh) + 1, x, rh, lh)
            ) in
        function
            (EMPTY, (h)) -> h
        |   ((h), EMPTY) -> h
        |   ((lh), (rh)) ->
                let lv = (findMin lh) in
                let rv = (findMin rh) in
                if(lv <= rv)
                then
                    let nlh = (deleteMin lh) in
                    shake(lv, nlh, rh)
                else
                    let nrh = (deleteMin rh) in
                    shake(rv, lh, nrh)
    )
