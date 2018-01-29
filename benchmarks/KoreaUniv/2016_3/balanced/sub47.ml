
  type mobile = branch * branch
  and branch = 
  | SimpleBranch of length * weight
  | CompoundBranch of length * mobile
  and length = int
  and weight = int
 

 let rec balanced : mobile -> bool
  = fun mob ->
		 match mob  with
				|(SimpleBranch(l1,w1),SimpleBranch(l2,w2))->
						if l1>=1 || l2>=1 || w1>=1 || w2>=1 
							then (if w1/l2=w2/l1 then true else false)
					  else raise NotImplemented
				|(CompoundBranch(l1,w1),SimpleBranch(l2,w2))->
						if eval(w1)/l2=w2/l1 then true else false
				|(SimpleBranch(l1,w1),CompoundBranch(l2,w2))->
						if w1/l2=eval(w2)/l1 then true else false
				|(CompoundBranch(l1,w1),CompoundBranch(l2,w2))->
						if eval(w1)/l2=eval(w2)/l1 then true else false
	and eval : mobile->int
	=fun m ->
		 match m with
				|(SimpleBranch(l1,w1),SimpleBranch(l2,w2))->w1+w2
				|(CompoundBranch(l1,w1),SimpleBranch(l2,w2))->eval(w1)+w2
				|(SimpleBranch(l1,w1),CompoundBranch(l2,w2))->w1+eval(w2)			
				|(CompoundBranch(l1,w1),CompoundBranch(l2,w2))->eval(w1)+eval(w2)
