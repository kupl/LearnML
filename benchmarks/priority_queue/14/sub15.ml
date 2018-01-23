(*
 * 컴퓨터공학부 2009-11690 김찬민
 * Homework 2 Exercise 2  *)

type heap = EMPTY | NODE of rank * value * heap * heap
and rank = int
and value = int

exception EmptyHeap

let rank = function EMPTY -> -1
                  | NODE (r, _, _, _) -> r

let shake = function (x,lh,rh) ->
    if (rank lh) >= (rank rh) then
        NODE(rank rh + 1, x, lh, rh)
    else
        NODE(rank lh + 1, x, rh, lh)

let rec merge ((h1:heap), (h2:heap)): heap =
    match (h1, h2) with
    | (EMPTY, other) -> other
    | (other, EMPTY) -> other
    | (NODE(_, v1, l1, r1), NODE(_, v2, l2, r2)) ->
        if v1 > v2 then
            shake (v2, l2, merge (h1, r2))
        else
            shake (v1, l1, merge (h2, r1))

let insert = function (x,h) -> merge(h, NODE(0,x,EMPTY,EMPTY))

let findMin = function EMPTY -> raise EmptyHeap
                     | NODE(_,x,_,_) -> x

let deleteMin = function EMPTY -> raise EmptyHeap
                       | NODE(_,x,lh,rh) -> merge(lh,rh)


(*
rank(shake(x,lh,rh)) = min(rank(lh),rank(rh)) + 1

Proposition
    merge_depth(h1,h2) := merge(h1, h2)를 호출하면 그 아래로 호출되는 merge 호출 횟수
    P(A,B) := merge_depth(h1,h2) <= rank(h1)+rank(h2)+2
    P holds for all A, B in finite heap

proof by induction)
base case)
    1) P(EMPTY, x) for any x
        merge_depth(EMPTY, x) = 0 <= -1 + -1 + 2 <= rank(EMPTY)+rank(x)+2
        therfore the inequality holds.
    2) P(x, EMPTY) for any x
        ditto.

induction hypothesis: for all A, B such that size(A) + size(B) < k, P(A,B) holds.
if there exists some a, b such that size(a) + size(b) = k, let's call it h1, h2 repectively.

1) (a is EMPTY) or (b is EMPTY): that was the base case

2) otherwise

    P(h1,r2) holds. (because size(r2) < size(h2))
    1 + merge_depth(h1, r2) <= 1 + (rank(h1)+rank(r2)+2) <= 1 + (rank(h1)+rank(h2)+1) = rank(h1)+rank(h2) + 2

    P(h2,r1) holds. (because size(r1) < size(h1))
    1 + merge_depth(h2, r1) <= 1 + (rank(h2)+rank(r1)+2) <= 1 + (rank(h2)+rank(h1)+1) = rank(h2)+rank(h1) + 2

    by constructive dilemma, P(h1,h2) holds

by 1) 2), whenever size(a) + size(b) = k, P(a,b) holds too.

therefore, P(A,B) holds for all A, B in heap by mathematical induction.

 *)
