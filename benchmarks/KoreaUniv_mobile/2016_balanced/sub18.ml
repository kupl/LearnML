exception NotImplemented
  type mobile = branch * branch
  and branch = 
  | SimpleBranch of length * weight
  | CompoundBranch of length * mobile
  and length = int
  and weight = int


	let rec cal_weight : branch->weight
	= fun br ->
	match br with
	|SimpleBranch(l, w) -> w
	|CompoundBranch(l, m) -> (match m with (lb,rb) -> (cal_weight lb)+(cal_weight rb))
	
	let rec cal_torque : branch->int
	=fun br -> match br with
	|SimpleBranch (l,w) -> l*w
	|CompoundBranch(l,m) -> (match m with (lb, rb) -> l*(cal_weight lb + cal_weight rb))


  let rec balanced : mobile -> bool
  = fun mob -> match mob with (lb, rb)->
(match (lb, rb) with 
|SimpleBranch(l1, w1), SimpleBranch(l2, w2) -> if l1*w1=l2*w2 then true
else false
|SimpleBranch(l1, w1),CompoundBranch(l2, m) -> (match m with (lb,rb) ->
(if (balanced m) then (if l2*(cal_weight(lb)+cal_weight(rb))=l1*w1 then true else false)else false))
|CompoundBranch(l1, m), SimpleBranch(l2, w2) -> (match m with (lb, rb) ->
	(if (balanced m) then (if l1*(cal_weight(lb)+cal_weight(rb)) = l2*w2 then true else false) else false))
|CompoundBranch(l1, m1), CompoundBranch(l2, m2) ->(match m1, m2 with (lb1, rb1),(lb2,rb2) ->
(if (balanced m1)&&(balanced m2) then (if l1*(cal_weight(lb1)+cal_weight(rb1))=l2*(cal_weight(lb2)+cal_weight(rb2)) then true else false) else false))
|_ -> raise NotImplemented
)
