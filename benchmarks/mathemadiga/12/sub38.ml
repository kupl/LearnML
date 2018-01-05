exception WrongNumber;;
exception FreeVariable;;
exception InvalidSigma;;

type exp = X
		|INT of int
		|REAL of float
		|ADD of exp * exp
		|SUB of exp * exp
		|MUL of exp * exp
		|DIV of exp * exp
		|SIGMA of exp * exp * exp
		|INTEGRAL of exp * exp * exp;;



let realRaiser expr = 
match expr with 
  REAL real -> (REAL (real +. 0.1))
  |INT integer -> (REAL ((float_of_int integer) +. 0.1))
  |_ -> raise WrongNumber;;


let sigmaIntFinder input = 
  match input with
    X -> raise FreeVariable
    |INT integer -> integer
    |_ -> raise InvalidSigma;;

let rec mathemadiga expr =
match expr with 
X -> raise FreeVariable
|INT integer -> float_of_int integer
|REAL real -> real
|ADD (first, second) -> (mathemadiga first) +. (mathemadiga second)
|SUB (first, second) -> (mathemadiga first) -. (mathemadiga second)
|MUL (first, second) -> (mathemadiga first) *. (mathemadiga second)
|DIV (first, second) -> (mathemadiga first) /. (mathemadiga second)

|SIGMA (firstInt, secondInt, expr) -> if (sigmaIntFinder firstInt) > (sigmaIntFinder secondInt) 
                                      then 0.0
                                      else (mathemadiga (SIGMA ( (INT ((sigmaIntFinder firstInt) + 1)), secondInt, expr))) +. 
                                        (numMaker (mathemadiga firstInt) expr)


|INTEGRAL (first, second, expr) -> if (mathemadiga first) > (mathemadiga second)
                                   then (mathemadiga (INTEGRAL (second, first, expr))) *. -1.0
                                   else if ((mathemadiga second) -. (mathemadiga first)) < 0.1 then (numMaker (mathemadiga first) expr) *. ((mathemadiga second) -. (mathemadiga first))
                                                                                                     
                                                                                                     
			else (mathemadiga (INTEGRAL ( (realRaiser first),second,expr))) +.
                          (numMaker (mathemadiga first) expr) *. 0.1


and numMaker floatIn expr = 
match expr with
X -> floatIn
|INT integer -> float_of_int integer
|REAL real -> real
|ADD (first, second) -> (numMaker floatIn first) +. (numMaker floatIn second)
|SUB (first, second) -> (numMaker floatIn first) -. (numMaker floatIn second)
|MUL (first, second) -> (numMaker floatIn first) *. (numMaker floatIn second)
|DIV (first, second) -> (numMaker floatIn first) /. (numMaker floatIn second)
|SIGMA (firstInt, secondInt, expr) -> (mathemadiga (SIGMA ((INT (int_of_float (numMaker floatIn firstInt))), (INT (int_of_float (numMaker floatIn secondInt))), expr)))
|INTEGRAL (first, second, expr) -> (mathemadiga (INTEGRAL ((REAL (numMaker floatIn first)), (REAL (numMaker floatIn second)), expr)));;



