
  type mobile = branch * branch
  and branch = 
  | SimpleBranch of length * weight
  | CompoundBranch of length * mobile
  and length = int
  and weight = int

let balanced : mobile -> bool
  = fun mob -> raise NotImplemented (* TODO *)

let rec adder e =
      match e with
      |SimpleBranch(n1,n2),SimpleBranch(n3,n4)->n2 + n4      
      |SimpleBranch(n1,n2),CompoundBranch(n3,e)->n2 + (adder e)
      |CompoundBranch(n1,e),SimpleBranch(n2,n3)->(adder e) + n3
      |CompoundBranch(n1,e1),CompoundBranch(n2,e2)->(adder e1)+(adder e2)
  
let balanced : mobile -> bool
  = fun mob -> match mob with
        |SimpleBranch(n1,n2),SimpleBranch(n3,n4)->(n1+n2) = (n3+n4)
        |SimpleBranch(n1,n2),CompoundBranch(n3,e)->(n1+n2) = (n3+(adder e))
        |CompoundBranch(n1,e),SimpleBranch(n2,n3)->(n1+(adder e)) = (n2+n3)
        |CompoundBranch(n1,e1),CompoundBranch(n2,e2)->
                                       (n1+(adder e1)) = (n2+(adder e2))
