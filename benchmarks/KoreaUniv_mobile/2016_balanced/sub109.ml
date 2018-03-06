
exception NotImplemented

  type mobile = branch * branch
  and branch = 
  | SimpleBranch of length * weight
  | CompoundBranch of length * mobile
  and length = int
  and weight = int


let rec cal_weight mob=
 match mob with
 |(SimpleBranch (a1,b1),SimpleBranch (a2,b2)) ->b1+b2
 |(SimpleBranch (a1,b1),CompoundBranch (a2,b2)) -> b1 + (cal_weight b2)
 |(CompoundBranch (a1,b1), SimpleBranch (a2,b2))-> (cal_weight b1) + b2
 |(CompoundBranch (a1,b1), CompoundBranch (a2,b2))-> 
(cal_weight b1)+(cal_weight b2)

and balanced : mobile -> bool
  = fun mob->
match mob with 
|(SimpleBranch (a1,b1),SimpleBranch (a2,b2)) ->
if (a1<0)||(b1<0)||(a2<0)||(b2<0) then raise NotImplemented
else a1*b1==a2*b2
|(SimpleBranch (a1,b1),CompoundBranch (a2,b2)) ->
 if (a1<0)||(b1<0)||(a2<0) then raise NotImplemented
else (a1*b1==a2*(cal_weight b2))&&(balanced b2)
|(CompoundBranch (a1,b1), SimpleBranch (a2,b2)) ->
 if (a1<0)||(a2<0)||(b2<0) then raise NotImplemented
else (a1*(cal_weight b1)==a2*b2)&&(balanced b1)
|(CompoundBranch (a1,b1), CompoundBranch (a2,b2)) ->
 if (a1<0)||(a2<0) then raise NotImplemented
else
 ((a1*(cal_weight b1))==(a2*(cal_weight b2)))&&(balanced b1)&&(balanced b2)
