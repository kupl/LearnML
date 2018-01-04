type crazy2= NIL
			 |ZERO of crazy2
			 |ONE of crazy2
			 |MONE of crazy2

let rec power num expo = match expo with
  							 0 -> 1
  							| 1 -> num
  							| any -> let x = power num (any/2) in
   								 x*x* (if any mod 2 = 0 then 1 else num)

let rec cval ((crz:crazy2),(i:int)) : int = match crz with
									NIL-> 0
									|ZERO x-> cval (x,i+1)
									|ONE x-> 1*(power 2 i)+ cval (x,i+1)
									|MONE x-> (-1)*(power 2 i)+ cval (x,i+1)

let crazy2val (crz:crazy2) : int = cval (crz,0)

let makelist ((x:int),(l:int list)) :int list= match l with
											 	[]-> x::[]
											 	|hd::tl-> x::l 
let flist= makelist (1,[])
let rec slist ((x:int),(l:int list)) :int list= 
		if (x>0) then slist ((x-1),(makelist(0,l)))
				 else l  
let rec dlist ((x:int list),(y:int list)) : int list list= [x;y] 
let rec mlist ((x:int list),(y:int list list)) : int list list= x::y


let rec finder ((x:int),(i:int)) : int= if (power 2 i > x) then i-1 else finder (x,i+1) 
let remain (x:int) : int= x -(power 2 (finder(x,0)))


let rec fl ((x:int),(l:int list list)) :int list list= 
if (x>0) then fl (remain x, mlist( slist (finder (x,0),flist), l ))
else l
let finalist (x:int):int list list= fl(x,[[];[]])

let ltoint (l:int list): int=List.fold_right (fun x acc -> x + acc * 10) l 0

let rec suml ((x:int list list),(sum:int)): int =
	match x with
	[]->sum 
	|hd::tl-> suml(tl,(sum+ ltoint hd))
let sumlist (x:int):int= suml (finalist x,0)

let rec digits ((n:int),(l:int list)) : int list =
  if (n>0) then digits(n/10,(n mod 10)::l)
  else l
let rec rl ((l:int list),(rel:int list)): int list=
	match l with 
	[]-> rel
	|hd::tl-> rl(tl,hd::rel) 
let relist (x:int) :int list= rl (digits (x,[]),[])

let abs(x: int):int =
  if (x < 0) then -x else x		

let replace l pos a  = List.mapi (fun i x -> if i = pos then a else x) l;;
let rec fillneg ((l:int list),(i:int)):int list= if (i<List.length l) then 
	match (List.nth l i) with	
	0-> fillneg (l,i+1)
	|1-> fillneg ((replace l i (-1)),i+1)
else l;;

let turNeg (x:int list):int list=fillneg (x,0);;

let rec translate (l:int list): crazy2= 
	match l with
	[]-> NIL
	|hd::tl-> match hd with 
				0-> ZERO(translate tl)
				|1-> ONE(translate tl)
				|(-1)-> MONE(translate tl) 
let translate2 (x:int): crazy2= 
	if (x<0) then 
	translate(turNeg(relist(sumlist (abs x))))	
	else 
	translate(relist(sumlist (abs x)))	

let crazy2add ((x:crazy2),(y:crazy2)) :crazy2= translate2(crazy2val(x)+crazy2val(y));;
