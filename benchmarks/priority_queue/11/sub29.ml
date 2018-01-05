type heap=EMPTY|NODE of rank*value*heap*heap
and rank = int
and value = int

exception EmptyHeap

let rank h=match h with NODE(r,_,_,_)->r|EMPTY->(-1)
let shake(x,lh,rh)= 
        let lr=rank lh in let rr=rank rh in
        if lr>=rr then NODE(rr+1,x,lh,rh) else NODE(lr+1,x,rh,lh) 

let rec merge(h1,h2)=(match h1 with
        |NODE(_,x1,lh,rh)->(match h2 with
                NODE(rn,x2,_,_)->
                if x1<=x2 then shake(x1,lh,merge(rh,h2)) else merge(h2,h1)
                |EMPTY->h1
        )|EMPTY->h2)

let insert(x,h)=merge(h,(NODE(0,x,EMPTY,EMPTY)))
let findMin h=match h with NODE(_,x,_,_)->x|EMPTY->raise EmptyHeap
let deleteMin h=match h with NODE(_,x,lh,rh)->merge(lh,rh)|EMPTY->raise EmptyHeap

(*
TEST CODE
let hc x=NODE(0,x,EMPTY,EMPTY)
let m(h1,h2)=merge(h1,h2)
let v1=m(hc 3,hc 5)
let v2=m(v1,hc 1)
let v3=m(hc 3,m(hc 2,hc 5))
let v4=m(v3,v3)
let v5=m(v5,v5)
*)
