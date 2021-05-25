type nat = ZERO | SUCC of nat 
    
let rec natadd x y = 
	match x with 
	| ZERO  -> y 
	| SUCC (p) ->  SUCC ( natadd p y) 

let rec printnat x = 
	if x = 0 then ZERO 
	else  SUCC(printnat(x-1))

let rec xint x = 
	match x with 
	| ZERO -> 0
	| SUCC (p) -> 1+ xint p

let rec yint y = 
	match y with 
	| ZERO -> 0
	| SUCC (p) -> 1+ yint p

let rec natmul x y = 
	printnat ((xint x)*(yint y))
