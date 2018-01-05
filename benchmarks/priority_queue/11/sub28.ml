type heap=EMPTY|NODE of rank*value*heap*heap
and rank = int
and value = int

exception EmptyHeap

let rank h=match h with NODE(r,_,_,_)->r|EMPTY->(-1)
let shake(x,lh,rh)= 
        let lr=rank lh in let rr=rank rh in
        if lr>=rr then NODE(rr+1,x,lh,rh) else NODE(lr+1,x,rh,lh) 

let rec merge(h1,h2)=(match h1 with
        NODE(_,x1,lh,rh)->(match h2 with
                NODE(rn,x2,_,_)->
                if x1<=x2 then(
                let lr=rank lh in let rr=rank rh in
                if (rn<=lr)&(rn<=rr) then NODE(rn+1,x1,merge(lh,rh),h2)
                else if (lr<=rn)&(lr<=rr) then NODE(lr+1,x1,merge(rh,h2),lh)
                else NODE(rr+1,x1,merge(lh,h2),rh)
                )else merge(h2,h1)
                |EMPTY->h1
        )|EMPTY->h2)

let insert(x,h)=merge(h,(NODE(0,x,EMPTY,EMPTY)))
let findMin h=match h with NODE(_,x,_,_)->x|EMPTY->raise EmptyHeap
let deleteMin h=match h with NODE(_,x,lh,rh)->merge(lh,rh)|EMPTY->raise EmptyHeap
