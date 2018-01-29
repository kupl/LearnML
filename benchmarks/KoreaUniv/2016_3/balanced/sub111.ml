
type mobile = branch * branch
  and branch = SimpleBranch of length * weight
             | CompoundBranch of length * mobile
  and length = int
  and weight = int
  
let rec getWeight
=fun x -> match x with
    | CompoundBranch(length,(lb,rb))->(getWeight lb)+(getWeight rb)
    | SimpleBranch(length,weight)->weight;;

let resultForBalance
=fun x -> match x with
    | CompoundBranch (length, (lb,rb)) -> length * ((getWeight lb) + (getWeight rb))
    | SimpleBranch (length, weight) -> length * weight;;

let rec balanced : mobile -> bool
=fun (lb,rb) ->match lb with
  | CompoundBranch (l1, (lb1, rb1)) ->  if(balanced (lb1, rb1) = false) then false
    else (match rb with
    | SimpleBranch (l2,w2) -> if ((resultForBalance lb) = (resultForBalance rb)) then true
    else false
    | CompoundBranch (l2, (lb2, rb2)) -> if (balanced (lb2, rb2) = false) then false
    else if ((resultForBalance lb) = (resultForBalance rb)) then true else false)
  | SimpleBranch (l1, w1) -> (match rb with
    | SimpleBranch (l2, w2) -> if ((resultForBalance lb) = (resultForBalance rb)) then true
    else false
   | CompoundBranch (l2,(lb1,rb1)) -> if((balanced (lb1, rb1) = true) && ((resultForBalance lb) = (resultForBalance rb))) then true else false);;                                                               
