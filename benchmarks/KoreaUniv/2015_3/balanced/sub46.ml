  type mobile = branch * branch
  and branch = SimpleBranch of length * weight
             | CompoundBranch of length * mobile
  and length = int
  and weight = int;;

  let rec weigh : mobile -> int
  =fun (left,right) -> match left with
    | SimpleBranch (_,lw) -> (match right with
        | SimpleBranch (_,rw) -> lw + rw
        | CompoundBranch (_,rother) -> lw + weigh rother)
    | CompoundBranch (_,lother) -> (match right with
        | SimpleBranch (_,rw) -> (weigh lother) + rw
        | CompoundBranch (_,rother) -> (weigh lother) + (weigh rother));;

  let rec balanced : mobile -> bool
  =fun (left,right) -> match left with
    | SimpleBranch (left_length,left_weight) -> (match right with
        | SimpleBranch (right_length,right_weight) -> left_length*left_weight = right_length*right_weight
        | CompoundBranch (right_length,right_other) -> (balanced right_other)
                                                    && (left_length*left_weight = right_length*(weigh right_other)))
    | CompoundBranch (left_length,left_other) -> (match right with
        | SimpleBranch (right_length,right_weight) -> (balanced left_other)
                                                   && (left_length*(weigh left_other) = right_length*right_weight)
        | CompoundBranch (right_length,right_other) -> (balanced left_other)
                                                    && (balanced right_other)
                                                    && (left_length*(weigh left_other) = right_length*(weigh right_other)));;
