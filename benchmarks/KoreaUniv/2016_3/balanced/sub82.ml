      type mobile = branch * branch
        and branch =
              | SimpleBranch of length * weight
                | CompoundBranch of length * mobile
                  and length = int
                    and weight = int

                      let rec addAllWeight
                        = fun mb ->
                              match mb with
                                | SimpleBranch(length, weight) -> weight
                                  | CompoundBranch(length, (sub_mb_left, sub_mb_right))
                                    -> (addAllWeight sub_mb_left) + (addAllWeight sub_mb_right)
                                      let rec multiply
                                        = fun mb ->
                                              match mb with
                                                | SimpleBranch(length,weight) -> length * weight
                                                  | CompoundBranch(length, (sub_mb_left, sub_mb_right)) -> length * (addAllWeight mb)


                                                    let rec balanced : mobile -> bool
                                                      = fun mob ->
                                                            match mob with
                                                              | (left, right) ->
                                                                            match left with
                                                                                  | SimpleBranch(length_simple_left, weight_simple_left) ->
                                                                                                    begin
                                                                                                                  match right with
                                                                                                                            | SimpleBranch(length_simple_right, weight_simple_right) -> if (multiply left) = (multiply right) then true else false
                                                                                                                                      | CompoundBranch(length_compound_right, (branch_left, branch_right)) -> if (balanced (branch_left, branch_right))  && (multiply left) = (multiply right) then true else false
                                                                                                                                                end
                                                                                                          | CompoundBranch(length_compound_left, (branch_one, branch_two)) ->
                                                                                                                            begin
                                                                                                                                        match right with
                                                                                                                                                  | SimpleBranch(length_simple_right, weight_simple_left) -> if (balanced (branch_one, branch_two)) && (multiply left) = (multiply right) then true else false
                                                                                                                                                            | CompoundBranch(length_compound_right, (branch_left, branch_right)) -> if (balanced (branch_one, branch_two)) && (balanced (branch_left,branch_right)) && (multiply left) = (multiply right) then true
                                                                                                                                                                      else false
                                                                                                                                                                                end
