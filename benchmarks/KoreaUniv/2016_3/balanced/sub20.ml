
  type mobile = branch * branch
  and branch = 
  | SimpleBranch of length * weight
  | CompoundBranch of length * mobile
  and length = int
  and weight = int

  let rec cal_mob : mobile -> int
  = fun mob -> match mob with
  | SimpleBranch (m1, m2),SimpleBranch (n1, n2) -> 
	if m1*m2=n1*n2 then m2+n2 else -1
  | SimpleBranch (m1, m2),CompoundBranch (n1, n2) ->
	if m1*m2=n1*(cal_mob n2) then m2+(cal_mob n2) else -1	
  | CompoundBranch (n1, n2),SimpleBranch (m1, m2) -> 
	if m1*m2=n1*(cal_mob n2) then m2+(cal_mob n2) else -1
  | CompoundBranch (m1, m2),CompoundBranch (n1, n2) -> 
	if m1*(cal_mob m2)=n1*(cal_mob n2) then (cal_mob m2)+(cal_mob n2) else -1
  
  let balanced : mobile -> bool
  = fun mob -> if (cal_mob mob)>(-1) then true else false