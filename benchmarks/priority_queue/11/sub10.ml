(* PL HW2-4, "우선큐"
   2007-11738
   알렉산더 *)

type heap = EMPTY | NODE of rank * value * heap * heap
and rank = int
and value = int

exception EmptyHeap

let rank h =
    match h with
      EMPTY -> -1
    | NODE (r, _, _, _) -> r

let shake (x, lh, rh) =
    if (rank lh) >= (rank rh)
        then NODE (rank rh+1, x, lh, rh)
    else     NODE (rank lh+1, x, rh, lh)

(*************************** merge function ************************************)
let rec merge (heap1, heap2) =
    (* balance merged heap; balance: heap -> heap *)
    let rec balance h =
        match h with
            EMPTY -> EMPTY
           |NODE (r, v, lh, rh) ->(if (rank lh) >= (rank rh) 
                                       then NODE (rank rh+1, v, balance lh, balance rh)
                                    else NODE (rank lh+1, v, balance rh, balance lh)
                                  )
    in
    (* merge subfunction; mergeInner: heap*heap -> heap *)
    let rec mergeInner (h1, h2) =
        match (h1, h2) with
         (EMPTY, EMPTY) -> raise EmptyHeap
        |(lh, EMPTY) -> lh
        |(EMPTY, rh) -> rh
        |((NODE (r1, v1, lh1, rh1)), 
          (NODE (r2, v2, lh2, rh2))) -> 
                    (if (r1 > r2) 
                        then NODE (r1, v1, lh1, (mergeInner (rh1, NODE(r2, v2, lh2, rh2))))
                    else NODE (r2, v2, lh2, (mergeInner (rh2, NODE(r1, v1, lh1, rh1))))
                    )
    in
    (* merge main *)
    balance (mergeInner (heap1, heap2))
(************************* end of merge function *******************************)

let findMin h =
    match h with
      EMPTY -> raise EmptyHeap
    | NODE (_, x, _, _) -> x

let insert (x, h) = 
    merge(h, NODE(0, x, EMPTY, EMPTY))

let deleteMin h =
    match h with
      EMPTY -> raise EmptyHeap
    | NODE (_, x, lh, rh) -> merge (lh, rh)



(***** TEST *****)
(*
let l = EMPTY
let ll = shake (1, l, l)
let nn = shake (2, ll, ll)
let ln = shake (3, ll, nn)
let nl = shake (4, nn, ll)
let lnn = shake (5, ln, nn)

let mer = merge(lnn, nl)
*)
