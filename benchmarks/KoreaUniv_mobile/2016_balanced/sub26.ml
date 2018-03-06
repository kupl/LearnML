
  type mobile = branch * branch
  and branch = 
  | SimpleBranch of length * weight
  | CompoundBranch of length * mobile
  and length = int
  and weight = int

let rec getWeight branch =
  match branch with
    | SimpleBranch (x, y) -> y
    | CompoundBranch (x, (left, right)) -> getWeight(left) + getWeight(right);;

let getTorque branch =
  match branch with
    | SimpleBranch (x, y) -> x * y
    | CompoundBranch (x, (left, right)) -> x * (getWeight(left) + getWeight(right));;

  let balanced : mobile -> bool
  = fun mob -> match mob with
               | (left, right) ->
                 if getTorque(left) = getTorque(right) then true else false;;