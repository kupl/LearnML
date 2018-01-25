type exp = X
         | INT of int
         | ADD of exp * exp
         | SUB of exp * exp
         | MUL of exp * exp
         | DIV of exp * exp
         | SIGMA of exp * exp * exp

let rec cal2 : exp*int -> int
=fun (e,n) -> match e with
INT f -> f
|X -> n
|ADD(f,g) -> (match f, g with
					X,X -> 2*n
					|INT f1, INT g1 -> f1+g1
					|INT f1, X -> f1+n
					|X, INT g1 -> n+g1
					|INT f1, _ -> f1+cal2(g,n)
					|_,INT g1 -> cal2(f,n)+g1
					|_,_ -> cal2(f,n)+cal2(g,n))
|SUB(f,g) -> (match f, g with
           X,X -> 0
           |INT f1, INT g1 -> f1-g1
           |INT f1, X -> f1-n
           |X, INT g1 -> n-g1
           |INT f1, _ -> f1-cal2(g,n)
           |_,INT g1 -> cal2(f,n)-g1
           |_,_ -> cal2(f,n)-cal2(g,n))
|MUL(f,g) -> (match f, g with
            X,X -> n*n
           |INT f1, INT g1 -> f1*g1
           |INT f1, X -> f1*n
           |X, INT g1 -> n*g1
           |INT f1, _ -> f1*cal2(g,n)
           |_,INT g1 -> cal2(f,n)*g1
           |_,_ -> cal2(f,n)*cal2(g,n))
|DIV(f,g) -> (match f, g with
            X,X -> 1
           |INT f1, INT g1 -> f1/g1
           |INT f1, X -> f1/n
           |X, INT g1 -> n/g1
           |INT f1, _ -> f1/cal2(g,n)
           |_,INT g1 -> cal2(f,n)/g1
           |_,_ -> cal2(f,n)/cal2(g,n))
|SIGMA(f,g,h) -> (match f,g with
INT f1, INT g1 -> if f1>g1 then 0
else cal2(h,g1)+cal2(SIGMA(f,INT(g1-1),h),0)
|_-> raise(Failure "error"))

let calculator : exp -> int
=fun e -> match e with
X-> raise(Failure "error")
|INT n -> n
|ADD(f,g) -> (match f,g with
					X,_ -> raise(Failure "error")
					|_,X -> raise(Failure "error")
					|_,_ -> cal2(e,0))
|SUB(f,g) -> (match f,g with
           X,_ -> raise(Failure "error")
           |_,X -> raise(Failure "error")
           |_,_ -> cal2(e,0))
|MUL(f,g) -> (match f,g with
           X,_ -> raise(Failure "error")
           |_,X -> raise(Failure "error")
           |_,_ -> cal2(e,0))
|DIV(f,g) -> (match f,g with
           X,_ -> raise(Failure "error")
           |_,X -> raise(Failure "error")
           |_,_ -> cal2(e,0))
|SIGMA(f,g,h) -> (match f,g with
						INT f1,INT g1 -> if f1>g1 then raise(Failure "error")
else cal2(e,0)
						|_,_ -> raise(Failure "error"))

