type exp = X
         | INT of int
         | ADD of exp * exp
         | SUB of exp * exp
         | MUL of exp * exp
         | DIV of exp * exp
         | SIGMA of exp * exp * exp

let rec expchange (x,cexp)=
	match x with 
				X->cexp
				|INT n->INT n
				|ADD(X,X)->ADD(cexp,cexp)
				|ADD(ee,X)|ADD(X,ee)->ADD(ee,cexp)
				|SUB(X,X)->SUB(cexp,cexp)
				|SUB(ee,X)->SUB(ee,cexp)
				|SUB(X,ee)->SUB(cexp,ee)
				|MUL(X,X)->MUL(cexp,cexp)
				|MUL(ee,X)|MUL(X,ee)->MUL(ee,cexp)
				|DIV(X,X)->DIV(cexp,cexp)
				|DIV(ee,X)->DIV(ee,cexp)
				|DIV(X,ee)->DIV(cexp,ee) 
				|ADD(x1,x2)->ADD(expchange(x1,cexp),expchange(x2,cexp))
				|SUB(x1,x2)->SUB(expchange(x1,cexp),expchange(x2,cexp))
				|MUL(x1,x2)->MUL(expchange(x1,cexp),expchange(x2,cexp))
				|DIV(x1,x2)->DIV(expchange(x1,cexp),expchange(x2,cexp))	
				
let rec change e=
	match e with
		INT n->n
		|ADD(e1,e2)->(change e1)+(change e2)
		|SUB(e1,e2)->(change e1)-(change e2)
		|MUL(e1,e2)->(change e1)*(change e2)
		|DIV(e1,e2)->if (change e2)=0 then raise(Failure "cannot not devide by 0") else (change e1)/(change e2)
		|SIGMA (e1,e2,e3)->if (change e1)>(change e2) then 0 else change(expchange (e3,e1))+change(SIGMA ((INT((change e1)+1),e2,e3)))
					
		    
	 	 			 	 	 	 	 				


let calculator exp=
	match exp with
		X->raise(Failure"no variable")
		|INT n->n
		|ADD(exp1,exp2)->change (ADD(exp1,exp2))
		|SUB(exp1,exp2)->change (SUB(exp1,exp2))
		|MUL(exp1,exp2)->change (MUL(exp1,exp2))
		|DIV(exp1,exp2)->change (DIV(exp1,exp2))
		|SIGMA(exp1,exp2,exp3)->change (SIGMA(exp1,exp2,exp3))

