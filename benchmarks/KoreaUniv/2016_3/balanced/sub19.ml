
type mobile = branch * branch
  and branch = SimpleBranch of length * weight
             | CompoundBranch of length * mobile
  and length = int
  and weight = int
  


let rec sumOfWeight
=fun br ->
   match br with
      | SimpleBranch(l,w)->w
      | CompoundBranch(l,(lb,rb))->(sumOfWeight lb)+(sumOfWeight rb);;


let multiple 
=fun br -> match br with
    | SimpleBranch (l, w) -> l * w
    | CompoundBranch (l, (lb,rb)) -> l * ((sumOfWeight lb) + (sumOfWeight rb));;

let rec balanced : mobile -> bool
=fun (lb,rb) ->match lb with
                            | SimpleBranch (l1, w1) -> (match rb with
                                                      | SimpleBranch (l2, w2) -> if ((multiple lb) = (multiple rb)) then true
                                                                                  else false
                                                      | CompoundBranch (l2,(lb1,rb1)) -> if(( balanced (lb1, rb1) = true) && ((multiple lb) = (multiple rb))) then true
                                                                                        else false)
                            | CompoundBranch (l1, (lb1, rb1)) ->  if( balanced (lb1, rb1) = false) then false
                                                                  else (match rb with
                                                                      | SimpleBranch (l2,w2) -> if ((multiple lb) = (multiple rb)) then true
                                                                                                else false
                                                                      | CompoundBranch (l2, (lb2, rb2)) -> if (balanced (lb2, rb2) = false) then false
                                                                                                           else if ((multiple lb) = (multiple rb)) then true
                                                                                                         else false);;