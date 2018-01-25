  type mobile = branch * branch
  and branch = SimpleBranch of length * weight
             | CompoundBranch of length * mobile
  and length = int
  and weight = int
  
  let rec cal : mobile -> int
  = fun (lb,rb) ->
  match lb,rb with
  | SimpleBranch(x1,y1), SimpleBranch(x2,y2) -> y1 + y2
  | SimpleBranch(x1,y1), CompoundBranch(x2,y2) -> if((x1*y1)==(x2*cal(y2))) then y1+cal(y2) else 0
  | CompoundBranch(x1,y1), SimpleBranch(x2,y2) -> if((x1*cal(y1))==(x2*y2)) then cal(y1)+y2 else 0
  | CompoundBranch(x1,y1), CompoundBranch(x2,y2) -> if((x1*cal(y1))==(x2*cal(y2))) then cal(y1)+cal(y2)   else 0

  let balanced : mobile -> bool
  =fun (lb,rb) -> 
  match lb,rb with
  | SimpleBranch(x1,y1), SimpleBranch(x2,y2) -> if (x1*y1)==(x2*y2) then true else false
  | SimpleBranch(x1,y1), CompoundBranch(x2,y2) -> if (x1*y1)==(x2*cal(y2)) then true else false
  | CompoundBranch(x1,y1), SimpleBranch(x2,y2) -> if (x1*cal(y1))==(x2*y2) then true else false
  | CompoundBranch(x1,y1), CompoundBranch(x2,y2) -> if (x1*cal(y1))==(x2*cal(y2)) then true else false
