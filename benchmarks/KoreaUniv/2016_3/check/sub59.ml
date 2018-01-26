type aexp = 
| Const of int 
| Var of string 
| Power of string*int
| Times of aexp list
| Sum of aexp list 

let rec diff : aexp * string ->aexp  =
fun f ->
match f with 
|(exp,str) -> let e = exp in
							let s0 = str in
							let rec diffsum : aexp list -> string -> aexp list 
							= fun l str ->
							match l with 
							|[]->[]
							|hd :: tl -> if hd = Const 0 then (diffsum tl s0)
										 	else  (diff (hd,s0))::(diffsum tl s0)
							
							in
							match e with 
							|Const n -> Const 0 
							|Var s1 -> if s1 = s0 then Const 1
										else Var s1 
							|Power(s2,n)->if s2 = s0 then Times [Const n;Power (s2,n-1)]
											else Power(s2,n)
							|Sum l -> Sum (diffsum l s0) 
							|Times l -> let rec swap : aexp -> aexp list -> aexp list 
											= fun e l0 -> 
											match l0 with 
											|[]->[]
											|hd :: tl -> if hd = e then (diff(e,s0))::tl
															else hd :: (swap e tl)
										in let rec difftimes : aexp list -> aexp list
											= fun l1 ->
											match l1 with 
											|[]->[]
											|hd :: tl -> (Times(swap hd l)) :: (difftimes tl)

										in Sum (difftimes l)


type mobile = branch * branch
and branch = SimpleBranch of length * weight
			|CompoundBranch of length * mobile
and length = int 
and weight = int 



let rec mobileweight m=
match m with
(l,r)->						let branchweight b = 
								match b with
								|SimpleBranch(_,w)->w
								|CompoundBranch(_,m)-> mobileweight m

								 in (branchweight l) + (branchweight r)

let rec balanced m =
match m with
(l,r)-> let branchbalanced b = 
			match b with 
			|SimpleBranch(_,_)->true
			|CompoundBranch(_,m1)->balanced m1
		in
			let torque b =	
			match b with 
			|SimpleBranch(l,w)->l*w
			|CompoundBranch(l,m)->l*(mobileweight m)
		in
			if ( torque l ) = (torque r ) && (branchbalanced l)=true && (branchbalanced r) = true then true
				else false 

  
