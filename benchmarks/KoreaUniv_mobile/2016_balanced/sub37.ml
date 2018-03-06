
  type mobile = branch * branch
  and branch = 
  | SimpleBranch of length * weight
  | CompoundBranch of length * mobile
  and length = int
  and weight = int

  let rec balanced : mobile -> bool
  = fun mob ->
begin
match mob with
| (SimpleBranch(l1,w1),SimpleBranch(l2,w2)) ->  if (l1*w1) = (l2*w2) then true else false
| (CompoundBranch(l1,mb),SimpleBranch(l2,w)) -> if balanced(mb) then if eval(mb)*l1 = l2*w then true else false else false
| (SimpleBranch(l1,w),CompoundBranch(l2,mb)) -> if balanced(mb) then if eval(mb)*l2 = l1*w then true else false else false
| (CompoundBranch(l1,mb1),CompoundBranch(l2,mb2)) ->if balanced (mb1) && balanced (mb2) then if eval(mb1)*l1 = eval(mb2)*l2 then true else false else false
end

and eval : mobile -> int
 = fun mob ->
match mob with
| (SimpleBranch(l1,w1),SimpleBranch(l2,w2)) -> w1+w2
| (CompoundBranch(l1,mb),SimpleBranch(l2,w2)) -> eval(mb)+w2
| (SimpleBranch(l1,w1),CompoundBranch(l2,mb)) -> w1 + eval(mb)
| (CompoundBranch(l1,mb1),CompoundBranch(l2,mb2)) -> eval(mb1) + eval(mb2)

