(* 프로그래밍언어 HW2 Exercise 4
   2009-11657 김동현 *)
	
(* List 모듈 사용법 - http://caml.inria.fr/pub/docs/manual-ocaml/libref/List.html 
   loop이나 재귀함수 대신 fold 함수 사용 *)
				
type exp = X 
         | INT of int 
         | REAL of float 
         | ADD of exp * exp 
         | SUB of exp * exp 
         | MUL of exp * exp 
         | DIV of exp * exp 
         | SIGMA of exp * exp * exp 
         | INTEGRAL of exp * exp * exp 

exception FreeVariable 
(* 변수 X가 SIGMA나 INTEGRAL에 묶이지 않는 경우 FreeVariable 예외 *)
		  
type expX = VALIDX of float
			    | INVALIDX (* SIGMA나 INTEGRAL에 묶이지 않는 잘못된 변수 *)

let handleX x =
	match x with
	| VALIDX v -> v
	| INVALIDX -> raise FreeVariable 
(* X의 처리. 계산 도중 잘못된 변수를 만날 경우 FreeVariable 예외 *)

(* 
 (* HW1 Exercise1 Sigma *)
let sigma (a, b, f) = 
    if a > b then 0
    else 
      let rec aux c d =
        if c >= b then d
        else aux (c + 1) (d + (f (c + 1)))
    in aux a (f a)
*)
        
let rec aux (e, x) = 
	match e with
	| X -> (match x with
	        | VALIDX v -> v
          | INVALIDX -> raise FreeVariable)  (* syntax - match 안 match 사용시 괄호 사용! *) 
  | INT i -> (float_of_int i)  (* 리턴 타입 float로 통일 *)
  | REAL r -> r 
  | ADD (e1, e2) -> aux (e1, x) +. aux (e2, x)  (* 실수의 사칙연산으로 정의 *)
  | SUB (e1, e2) -> aux (e1, x) -. aux (e2, x) 
  | MUL (e1, e2) -> aux (e1, x) *. aux (e2, x) 
  | DIV (e1, e2) -> aux (e1, x) /. aux (e2, x)        
  | SIGMA (a, b, f) -> if aux (a, x) > aux (b, x) then 0.0
		                   (* SIGMA 계산시 n = a to b 라 할 때 a > b 인 경우 0을 리턴 *) 
                       else
												let rec handleSigmaInput (i, l) =
													if i > l then []  (* base case *)
				                  else (VALIDX i)::(handleSigmaInput ((i +. 1.0), l)) in
			                  let sigmaInputList = (handleSigmaInput (aux (a, x), aux (b, x))) in
			                  (* a, b 입력으로 실수가 들어오는 경우의 처리 ? *)
												(* List.fold_right (+.) 0.0 (List.map (fun x -> aux (f, x)) sigmaInputList) *)
			                  List.fold_left (+.) 0.0 (List.map (fun x -> aux (f, x)) sigmaInputList)
	  (* val fold_left : ('a -> 'b -> 'a) -> 'a -> 'b list -> 'a
         List.fold_left f a [b1; ...; bn] is f (... (f (f a b1) b2) ...) bn. 				
	     val map : ('a -> 'b) -> 'a list -> 'b list
         List.map f [a1; ...; an] applies function f to a1, ..., an, and builds the list [f a1; ...; f an] with the results returned by f. Not tail-recursive. *)
	| INTEGRAL (a, b, f) -> if aux (a, x) = aux (b, x) then 0.0
		                      (* a = b인 경우 0을 리턴 *)
		                      else if aux (a, x) < aux (b, x) then														
														if (aux (b, x) -. aux (a, x)) < 0.1 then 0.0
														(* 시작과 끝 값의 차가 0.1 미만인 경우 0을 리턴 *)
														else													
														  (* handleSigmaInput과 같은 방식으로 *)		
			                        let rec handleIntegralInput (i, l) =
														  	if i > l then []  (* base case *)
				                        else (VALIDX i)::(handleIntegralInput ((i +. 0.1), l)) in (* dx는 0.1로 *)		
			                        let integralInputList = handleIntegralInput (aux (a, x), aux (b, x)) in
			                        let (pa, pb) = ((List.hd (List.rev integralInputList)), (List.rev (List.tl (List.rev integralInputList)))) in
			                        let fValueList = (List.map (fun x -> aux (f, x)) pb) in
													    (List.fold_left (+.) 0.0 (List.map (( *.) 0.1) fValueList)) +. ((aux (b, x) -. (handleX pa)) *. (aux (f, pa)))
		(* val hd : 'a list -> 'a
		     Return the first element of the given list. Raise Failure "hd" if the list is empty.
		   val tl : 'a list -> 'a list
         Return the given list without its first element. Raise Failure "tl" if the list is empty.
		   val rev : 'a list -> 'a list
         List reversal. *) 			
		                      else (-1.0) *. (aux (INTEGRAL (b, a, f), x))
		                      (* a > b인 경우. INTEGRAL(a, b, f) = -INTEGRAL(b, a, f) *) 
									 
let galculator exp = aux (exp, INVALIDX) 
