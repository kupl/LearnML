
  type mobile = branch * branch
  and branch = 
  | SimpleBranch of length * weight
  | CompoundBranch of length * mobile
  and length = int
  and weight = int

  let rec getTotalW : mobile -> int
 = fun submob ->
 match submob with
 | (SimpleBranch(_, wgh1), SimpleBranch(_, wgh2)) -> wgh1 + wgh2
 | (SimpleBranch(_, wgh1), CompoundBranch(_, submob2)) -> wgh1 + getTotalW(submob2)
 | (CompoundBranch(_, submob1), SimpleBranch(_, wgh2)) -> getTotalW(submob1)+wgh2
 | (CompoundBranch(_, submob1), CompoundBranch(_, submob2)) -> getTotalW(submob1) + getTotalW(submob2)
 
  let rec balanced : mobile -> bool
  = fun mob -> 
 match mob with
 | (SimpleBranch(len1, wgh1),SimpleBranch(len2, wgh2)) -> (len1*wgh1)=(len2*wgh2)
 | (SimpleBranch(len1, wgh1),CompoundBranch(len2, submob2))
	-> (balanced submob2) && ((len1*wgh1)=len2*getTotalW(submob2))
 | (CompoundBranch(len1, submob1), SimpleBranch(len2, wgh2))
	-> (balanced submob1) && (len1*getTotalW(submob1)=(len2*wgh2))
 | (CompoundBranch(len1, submob1), CompoundBranch(len2, submob2))
	-> (balanced submob1) && (balanced submob2) && (len1*getTotalW(submob1)=len2*getTotalW(submob2))