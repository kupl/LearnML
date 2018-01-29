
  type mobile = branch * branch
  and branch = 
  | SimpleBranch of length * weight
  | CompoundBranch of length * mobile
  and length = int
  and weight = int

  let rec balanced : mobile -> bool
  = fun mob -> 
    let rec weightOfMobile (b1, b2) = 
      match b1 with
      | SimpleBranch(l1,w1) -> 
        begin
        match b2 with
        | SimpleBranch(l2,w2) -> w1 + w2
        | CompoundBranch(l2,m2) -> w1 + weightOfMobile m2
        end
      | CompoundBranch(l1,m1) -> 
        begin
        match b2 with
        | SimpleBranch(l2,w2) -> weightOfMobile m1 + w2
        | CompoundBranch(l2,m2) -> weightOfMobile m1 + weightOfMobile m2
        end
      in match mob with
        | (b1,b2) ->
          match b1 with
            | SimpleBranch(l1,w1) -> 
              begin
              match b2 with
              | SimpleBranch(l2,w2) -> (l1 * w2) = (l2 * w2)
              | CompoundBranch(l2,m2) -> 
                begin
                if(balanced m2) then (l1 * w1) = (l2 * weightOfMobile m2)
                else false
                end
              end
            | CompoundBranch(l1,m1) -> 
              begin
              if(balanced m1) then
                match b2 with
                | SimpleBranch(l2,w2) -> (l1 * weightOfMobile m1) = (l2 * w2)
                | CompoundBranch(l2,m2) -> 
                  begin
                  if(balanced m2) then (l1 * weightOfMobile m1) = (l2 * weightOfMobile m2)
                  else false
                  end
              else false
              end