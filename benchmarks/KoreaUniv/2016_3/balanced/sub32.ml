
  type mobile = branch * branch
  and branch = 
  | SimpleBranch of length * weight
  | CompoundBranch of length * mobile
  and length = int
  and weight = int

let rec m_weight : branch -> weight
= fun br ->
         match br with
         | SimpleBranch (l, w) -> w
         | CompoundBranch (l, m) -> (match m with
                                      (lb, rb) -> (m_weight lb) + (m_weight rb));;

let rec m_torque : branch -> int
= fun t ->
         match t with
         | SimpleBranch (l, w) -> l * w
         | CompoundBranch (l, m) -> (match m with
                                     (lb, rb) -> if (m_torque lb) = (m_torque rb)
                                                then (l * ((m_weight lb) + (m_weight rb)))
                                                else (0));;

let balanced : mobile -> bool
= fun mob -> 
         match mob with
         (lb, rb) -> if(m_torque lb) = (m_torque rb) then true else false;;
