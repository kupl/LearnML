exception NotImplemented
  type mobile = branch * branch
  and branch = 
  | SimpleBranch of length * weight
  | CompoundBranch of length * mobile
  and length = int
  and weight = int

let rec sum : mobile -> int
	=fun mob ->
match mob with

|SimpleBranch(a,b), SimpleBranch(c,d) -> b+d
|SimpleBranch(a,b), CompoundBranch(c,m) -> b + (sum m)
|CompoundBranch(c,m), SimpleBranch(a,b) -> b + (sum m)
|CompoundBranch(a,m1), CompoundBranch(b,m2) -> (sum m1) + (sum m2)


let rec balanced : mobile -> bool
	= fun mob ->
match mob with
|SimpleBranch(a,b), SimpleBranch(c,d) -> 
if (a<0)||(b<0)||(c<0)||(d<0) then raise NotImplemented 
else if  (a*b==c*d) then true else false


|SimpleBranch(a,b), CompoundBranch(c,m) ->
if (a<0)||(b<0)||(c<0) then raise NotImplemented 
else if ((sum m)*c == a*b)&&(balanced m) then true else false

|CompoundBranch(c,m), SimpleBranch (a,b) ->
if (a<0)||(b<0)||(c<0) then raise NotImplemented
else if ((sum m)*c == a*b)&&(balanced m) then true else false

|CompoundBranch(a,m1), CompoundBranch(b,m2) ->
if (a<0)||(b<0) then raise NotImplemented 
else if ((sum m1)*a == (sum m2)*b)&&(balanced m1)&&(balanced m2) then true
else false
