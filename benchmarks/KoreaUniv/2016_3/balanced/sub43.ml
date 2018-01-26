
  type mobile = branch * branch
  and branch = 
  | SimpleBranch of length * weight
  | CompoundBranch of length * mobile
  and length = int
  and weight = int

  let rec total_weight : mobile -> int
  = fun mob -> match mob with
  | (b1, b2) -> match b1, b2 with
  | SimpleBranch (len1, wei1), SimpleBranch (len2, wei2) -> wei1 + wei2
  | SimpleBranch (len1, wei1), CompoundBranch (len2, mob2) -> wei1 + total_weight mob2
  | CompoundBranch (len1, mob1), SimpleBranch (len2, wei2) -> total_weight mob1 + wei2
  | CompoundBranch (len1, mob1), CompoundBranch (len2, mob2) -> total_weight mob1 + total_weight mob2
  | _ , SimpleBranch (len2, wei2) -> wei2
  | _ , CompoundBranch (len2, mob2) -> total_weight mob2
  | SimpleBranch (len1, wei1), _ -> wei1
  | CompoundBranch (len1, mob1), _ -> total_weight mob1
  | _ , _ -> 0 (* TODO *)

  let rec balanced : mobile -> bool
  = fun mob -> match mob with
  | (b1, b2) -> match b1, b2 with
  | SimpleBranch (len1, wei1), SimpleBranch (len2, wei2) -> if len1 * wei1 = len2 * wei2 then true else false
  | SimpleBranch (len1, wei1), CompoundBranch (len2, mob2) -> if len1 * wei1 = len2 * total_weight mob2 && balanced mob2 = true then true else false
  | CompoundBranch (len1, mob1), SimpleBranch (len2, wei2) -> if len1 * total_weight mob1 = len2 * wei2 && balanced mob1 = true then true else false
  | CompoundBranch (len1, mob1), CompoundBranch (len2, mob2) -> if len1 * total_weight mob1 = len2 * total_weight mob2 && balanced mob1 = true && balanced mob2 = true then true else false
  | _ , SimpleBranch (len2, wei2) -> false
  | _ , CompoundBranch (len2, mob2) -> false
  | SimpleBranch (len1, wei1), _ -> false
  | CompoundBranch (len1, mob1), _ -> false
  | _ , _ -> false (* TODO *)