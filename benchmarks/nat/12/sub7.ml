type nat = ZERO | SUCC of nat

let natadd: nat*nat->nat= fun(n1, n2)->
  let rec icrCnt(cnt, max, n)=
  if cnt = max then n
  else icrCnt(SUCC cnt, max, SUCC n) in

  match (n1, n2) with
  (ZERO, ZERO)->ZERO
  |(ZERO, SUCC n)-> n2
  |(SUCC n, ZERO)-> n1
  |(SUCC sn1, SUCC sn2)-> icrCnt(ZERO, n1, n2)

let natmul: nat*nat->nat= fun(n1, n2)->
  let rec addCnt(cnt, loop, n, res)=
    if cnt = loop then res
    else addCnt(SUCC cnt, loop, n, natadd(res,n)) in
  match (n1,n2) with
  (ZERO,_)->ZERO
  |(_,ZERO)->ZERO
  |(SUCC sn1, SUCC sn2)->addCnt(ZERO, sn1, n2, n2)

let nat2int n = 
   let rec nat_to_int :nat*int->int = fun (n, i)->
     match (n,i) with
     |(ZERO,i)->i
     |(SUCC sn,i)-> 1+nat_to_int (sn, i) in
   nat_to_int(n, 0)