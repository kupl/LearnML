  type mobile = branch * branch
  and branch = SimpleBranch of length * weight
             | CompoundBranch of length * mobile
  and length = int
  and weight = int

let rec weighbr : branch -> int
= fun b -> match b with
| SimpleBranch(len, wg) -> wg
| CompoundBranch(len, mb) -> weighmb(mb)

and weighmb : mobile -> int
= fun (lb,rb) -> weighbr(lb) + weighbr(rb)

let rec torqbr : branch -> int
= fun b -> match b with
| SimpleBranch(len, wg) -> len * wg
| CompoundBranch(len, mb) -> len * weighmb(mb)

let rec balbr : branch -> bool
= fun b -> match b with
| SimpleBranch(len, wg) -> true
| CompoundBranch(len, mb) -> balmb(mb)

and balmb : mobile -> bool
=fun (lb, rb) -> 
if (torqbr(lb) == torqbr(rb)) then balbr(lb)&&balbr(rb)
else false 
  
  let balanced : mobile -> bool
  =fun (lb,rb) -> balmb(lb,rb)
