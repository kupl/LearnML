type heap = EMPTY
| NODE of rank * value * heap * heap
and rank = int
and value = int;;

exception EmptyHeap;;

let rank = function EMPTY -> -1 | NODE (r, _, _, _) -> r;;
let findMin = function EMPTY -> raise EmptyHeap | NODE(_, x, _, _) -> x;;
let leftHeap = function EMPTY -> raise EmptyHeap | NODE(_, _, leftHeap, _) -> leftHeap;;
let rightHeap = function EMPTY -> raise EmptyHeap | NODE(_, _, _, rightHeap) -> rightHeap;;

let shake = function (x,lh,rh) ->
if (rank lh) >= (rank rh)
    then NODE(rank rh + 1, x, lh, rh)
    else NODE(rank lh + 1, x, rh, lh);;

let rec merge heaps =
    match heaps with
    | (EMPTY, EMPTY) -> EMPTY
    | (EMPTY, heap1) -> heap1
    | (heap1, EMPTY) -> heap1
    | (heap1, heap2) ->
        if (findMin heap1) <= (findMin heap2)
            then shake((findMin heap1), heap2, (merge ((leftHeap heap1), (rightHeap heap1))))
            else shake((findMin heap2), heap1, (merge ((leftHeap heap2), (rightHeap heap2))));;

let insert = function (x,h) -> merge(h, NODE(0,x,EMPTY,EMPTY));;
let deleteMin = function EMPTY -> raise EmptyHeap | NODE(_,x,lh,rh) -> merge(lh,rh);;

(*
let myHeap = insert(3, insert(1, insert(2, insert(7, insert(6, insert(4, insert(5, EMPTY)))))));;
let _ = print_int( rank(myHeap) );;
let _ = print_int( findMin(myHeap) );;

let myHeap2 = deleteMin(myHeap);;
let _ = print_int( rank(myHeap2) );;
let _ = print_int( findMin(myHeap2) );;

let t2 = insert (1, insert (10, insert(30,EMPTY)));;
let t3 = insert (25, insert (34, insert(9, insert (11,EMPTY))));;
let t5 = merge (t2,t3);;
print_int (findMin t5);;
print_int (findMin (deleteMin t5));;
*)
