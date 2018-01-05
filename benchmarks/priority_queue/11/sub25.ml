type heap=EMPTY|NODE of rank*value*heap*heap
and rank = int
and value = int

exception EmptyHeap
exception EmptyList

let rank h=match h with NODE(r,_,_,_)=r|EMPTY->0
let insert(x,h)=merge(h,NODE(0,x,EMPTY,EMPTY))
let findMin h=match h with NODE(_,x,_,_)->x|EMPTY->raise EmptyHeap
let deleteMin h=match h with NODE(_,x,lh,rh)->merge(lh,rh)|EMPTY->raise EmptyHeap

let rec grade h=match h with NODE(_,_,_,rh)->1+(grade rh)|EMPTY->0
let rec min_grade l=if List.tl(l)!=[] then if
        grade(List.hd(l))<grade(List.hd(List.tl(l))) then
                min_grade(List.hd(l)::List.tl(List.tl(l))) else
                        min_grade(List.tl(l))
        else (if l!=[] then grade(List.hd(l)) else raise EmptyList)

let rec merge h1 h2=
        (match h1 with
        NODE(rn1,x1,l1,r1)->(match h2 with
                EMPTY->h1
                |NODE(rn2,x2,l2,r2)->(
                        if x1>x2 then (
                                if grade(x2)>grade(l1) then (
                                        if grade(l1)>grade(r1) then
                                                NODE(rn1+rn2,x1,merge(x2,l1),r1)
                                        else NODE(rn1+rn2,x1,merge(x2,r1),l1))
                                else (if grade(x2)>grade(r1) then NODE(rn1+rn2,x1,merge(x2,l1),r1)
                                        else NODE(rn1+rn2,x1,merge(l1,r1),x2))

                        else (
                                if grade(x1)>grade(l2) then (
                                        if grade(l2)>grade(r2) then
                                                NODE(rn1+rn2,x2,merge(x1,l2),r2)
                                        else NODE(rn1+rn2,x2,merge(x1,r2),l2))
                                else (if grade(x1)>grade(r2) then
                                        NODE(rn1+rn2,x2,merge(x1,l2),r2)
                                        else NODE(rn1+rn2,x2,merge(l2,r2),x1))
                        )
                )
        EMPTY->h2
        )
