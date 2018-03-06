  type mobile = branch * branch
  and branch = SimpleBranch of length * weight
             | CompoundBranch of length * mobile
  and length = int
  and weight = int
  
  let rec getTorque : mobile -> int
  =fun (lb,rb) -> match lb,rb with
  | SimpleBranch(x1,y1),CompoundBranch(x2,y2) -> y1+(getTorque y2)
  | CompoundBranch(x1,y1),SimpleBranch(x2,y2) -> (getTorque y1)+y2
  | SimpleBranch(x1,y1),SimpleBranch(x2,y2) -> y1+y2
  | CompoundBranch(x1,y1),CompoundBranch(x2,y2) -> (getTorque y1)+(getTorque y2)
  
  let balanced : mobile -> bool
  =fun (lb,rb) -> match lb,rb with
  | SimpleBranch(x1,y1),CompoundBranch(x2,y2) -> ((x1*y1)=(x2*(getTorque y2)))
  | CompoundBranch(x1,y1),SimpleBranch(x2,y2) -> ((x1*(getTorque y1))=(x2*y2))
  | SimpleBranch(x1,y1),SimpleBranch(x2,y2) -> ((x1*y1)=(x2*y2))
  | CompoundBranch(x1,y1),CompoundBranch(x2,y2) ->
	((x1*(getTorque y1))=(x2*(getTorque y2)))
