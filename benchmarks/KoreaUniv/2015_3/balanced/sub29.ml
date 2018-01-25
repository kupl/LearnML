  type mobile = branch * branch
  and branch = SimpleBranch of length * weight
             | CompoundBranch of length * mobile
  and length = int
  and weight = int

let rec weightOf branch=
  match branch with
    SimpleBranch(l,w)->w
    |CompoundBranch(l,(lb,rb))->(weightOf lb)+(weightOf rb)
    

let rec balanced : mobile -> bool
 =fun (lb,rb)->match lb with
      SimpleBranch(l,w)->(match rb with
            SimpleBranch(l1,w1)->if l*w=l1*w1 then true else false
            |CompoundBranch(l1,(rb1,rb2))->if (balanced (rb1,rb2))&&((weightOf rb)*l1=l*w) then true else false)    
      |CompoundBranch(l,(b1,b2))->if balanced (b1,b2) then (match rb with
                      SimpleBranch(l1,w1)->if (l*weightOf lb)=l1*w1 then true else false
                      |CompoundBranch(l1,(rb1,rb2))->if (balanced (rb1,rb2))&&((weightOf rb)*l1=(weightOf lb)*l) then true else false) else false 

    
    
