  type mobile = branch * branch
  and branch = SimpleBranch of length * weight
             | CompoundBranch of length * mobile
  and length = int
  and weight = int

  let len b = match b with
    | SimpleBranch(length, _) -> length
    | CompoundBranch(length, _) -> length
  ;;

  let left b = match b with
  | (left, _) -> left
  ;;
 
  let right b = match b with
  | (_, right) -> right
  ;;

  let rec calc b = match b with
    | SimpleBranch (len, weight) -> weight
    | CompoundBranch (len, mobile) -> (calc (left mobile)) + (calc (right
    mobile))
  ;;
  let balanced : mobile -> bool
  =fun (lb,rb) -> ((len lb) * (calc lb) == (len rb) * (calc rb))
  ;;

