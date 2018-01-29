
  type mobile = branch * branch
  and branch = 
  | SimpleBranch of length * weight
  | CompoundBranch of length * mobile
  and length = int
  and weight = int

  let get_length br = 
    match br with 
      | SimpleBranch (length, weight) -> length
      | CompoundBranch (length, mob) -> length

  let rec balanced : mobile -> bool
  = fun mob -> 
    let (left, right) = mob in 
      let (left_balanced, left_weight) = balanced_helper left in
        let (right_balanced, right_weight) = balanced_helper right in 
          let left_length = get_length left in
            let right_length = get_length right in
             left_balanced && right_balanced && left_weight*left_length == right_weight*right_length
  
  and balanced_helper br =
    match br with
      | SimpleBranch (length, weight) -> (true, weight)
      | CompoundBranch (length, mob) -> 
        let (left, right) = mob in 
          let (left_balanced, left_weight) = balanced_helper left in
            let (right_balanced, right_weight) = balanced_helper right in 
              let left_length = get_length left in
                let right_length = get_length right in
                  (left_balanced && right_balanced && left_weight*left_length == right_weight*right_length, left_weight + right_weight);;
