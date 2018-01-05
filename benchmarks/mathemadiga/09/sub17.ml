exception FreeVariable
exception InvalidSigma 
exception DivideByZero

type exp = X
         | INT of int
         | REAL of float
         | ADD of exp*exp
         | SUB of exp*exp 
         | MUL of exp*exp
         | DIV of exp*exp
         | SIGMA of exp*exp*exp
         | INTEGRAL of exp*exp*exp

let rec mathemadiga e =
      match e with 
      |X -> raise FreeVariable
      |INT e1 -> float_of_int(e1)
      |REAL e1 ->e1
      |ADD(e1,e2)-> mathemadiga(e1)+.mathemadiga(e2)
      |SUB(e1,e2)-> mathemadiga(e1)-.mathemadiga(e2)
      |MUL(e1,e2)-> mathemadiga (e1) *. mathemadiga (e2)
      |DIV(e1,e2)-> if mathemadiga(e2) = 0.0 then raise DivideByZero
          else mathemadiga  (e1) /. mathemadiga (e2)
      |SIGMA(e1, e2,e3)-> 
             let rec subMath f= 
                  match f with 
                |X,b -> b
                |INT e1, b -> float_of_int(e1)
                |REAL e1, b -> e1
                |ADD(e1,e2),b -> subMath(e1,b)+.subMath(e2,b)
                |SUB(e1,e2),b->subMath(e1,b)-.subMath(e2,b)
                |MUL(e1,e2),b-> subMath(e1,b)*.subMath(e2,b)
                |DIV(e1,e2),b->if subMath(e2,b) = 0.0 then raise DivideByZero
         			 else subMath (e1,b) /. subMath (e2,b)
                |SIGMA(e1,e2,e3),b-> 
 			if subMath (e1,b) > subMath (e2,b) then
                                 raise InvalidSigma
                         else if subMath (e2,b) -. subMath (e1,b) <1.0  then 
                              subMath(e3,subMath (e1,b))
                          else 
                          subMath(e3,subMath (e1,b))+. subMath (SIGMA(REAL(subMath (e1,b)+.1.0),e2,e3),b)
                |INTEGRAL(e1,e2,e3),b-> if subMath(e1,b)=subMath(e2,b) then
                               0.0
               else if subMath(e1,b)>subMath(e2,b) then
                          -.subMath(INTEGRAL(e2,e1,e3),b) 
                else if subMath(e2,b) -. subMath(e1,b) < 0.1  then 
                          subMath(e3,subMath(e1,b))*.(subMath(e2,b) -. subMath(e1,b))
                          else 
                          subMath(e3,subMath(e1,b))*.0.1+. subMath(INTEGRAL(REAL(subMath(e1,b)+.0.1),e2,e3),b)
        
        in if mathemadiga(e1)>mathemadiga(e2) then
                                 raise InvalidSigma
                else if mathemadiga(e2) -. mathemadiga(e1) < 1.0  then 
                          subMath(e3,mathemadiga(e1))
                          else 
                          subMath(e3,mathemadiga(e1))+. mathemadiga(SIGMA(REAL(mathemadiga(e1)+.1.0),e2,e3))
      |INTEGRAL(e1,e2,e3)-> 
		let rec sub2Math f= 
                  match f with 
                |X,b -> b
                |INT e1, b -> float_of_int(e1)
                |REAL e1, b -> e1
                |ADD(e1,e2),b -> sub2Math(e1,b)+.sub2Math(e2,b)
                |SUB(e1,e2),b->sub2Math(e1,b)-.sub2Math(e2,b)
                |MUL(e1,e2),b-> sub2Math(e1,b)*.sub2Math(e2,b)
                |DIV(e1,e2),b->if sub2Math(e2,b) = 0.0 then raise DivideByZero
         			 else sub2Math (e1,b) /. sub2Math (e2,b)
                |SIGMA(e1,e2,e3),b-> 
 			if sub2Math (e1,b) > sub2Math (e2,b) then
                                 raise InvalidSigma
                         else if sub2Math (e2,b) -. sub2Math (e1,b) <1.0  then 
                              sub2Math(e3,sub2Math (e1,b))
                          else 
                          sub2Math(e3,sub2Math (e1,b))+. sub2Math (SIGMA(REAL(sub2Math (e1,b)+.1.0),e2,e3),b)
                |INTEGRAL(e1,e2,e3),b-> 
		 if sub2Math(e1,b)=sub2Math(e2,b) then
                               0.0
               else if sub2Math(e1,b)>sub2Math(e2,b) then 
                          -.sub2Math(INTEGRAL(e2,e1,e3),b) 
                else if sub2Math(e2,b) -. sub2Math(e1,b) < 0.1  then 
                          sub2Math(e3,sub2Math(e1,b))*.(sub2Math(e2,b) -. sub2Math(e1,b))
                          else 
                          sub2Math(e3,sub2Math(e1,b))*.0.1+. sub2Math(INTEGRAL(REAL(sub2Math(e1,b)+.0.1),e2,e3),b)
        
                                      
        in  if mathemadiga(e1)=mathemadiga(e2) then
                               0.0
               else if mathemadiga(e1)>mathemadiga(e2) then 
                          -.mathemadiga(INTEGRAL(e2,e1,e3)) 
                else if mathemadiga(e2) -. mathemadiga(e1) < 0.1  then 
                          sub2Math(e3,mathemadiga(e1))*.(mathemadiga(e2) -. mathemadiga(e1))
                          else 
                          sub2Math(e3,mathemadiga(e1))*.0.1+. mathemadiga(INTEGRAL(REAL(mathemadiga(e1)+.0.1),e2,e3))