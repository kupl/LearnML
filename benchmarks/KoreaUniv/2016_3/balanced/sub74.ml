
  type mobile = branch * branch
  and branch =
  | SimpleBranch of length * weight
  | CompoundBranch of length * mobile
  and length = int
  and weight = int

  let rec balanced : mobile -> bool
  = fun mob->
match mob with
| SimpleBranch(ll,lw),SimpleBranch(rl,rw)->
if ll*lw=rl*rw then true
else false
| SimpleBranch(ll,lw),CompoundBranch(rl,rw)->
if balanced rw=true then
(if ll*lw=rl*(mob_weight rw) then true
else false)
else false
| CompoundBranch(ll,lw),SimpleBranch(rl,rw)->
if balanced lw=true then
(if ll*(mob_weight lw)=rl*rw then true
else false)
else false
| CompoundBranch(ll,lw),CompoundBranch(rl,rw)->
if (balanced lw=true)&&(balanced rw=true) then
(if ll*(mob_weight lw)=rl*(mob_weight rw) then true
else false)
else false
|_ ->raise (Failure "wrong expression")

and mob_weight :mobile->int
=fun mob->
match mob with
| SimpleBranch(ll,lw),SimpleBranch(rl,rw)->lw+rw
| SimpleBranch(ll,lw),CompoundBranch(rl,rw)->lw+(mob_weight rw)
| CompoundBranch(ll,lw),SimpleBranch(rl,rw)->(mob_weight lw)+rw
| CompoundBranch(ll,lw),CompoundBranch(rl,rw)->(mob_weight lw)+(mob_weight rw);;
