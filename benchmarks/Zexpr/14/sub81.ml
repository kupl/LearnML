module type ZEXPR = sig 
  exception Error of string 
  type id = string 
  type expr = 
    | NUM of int 
    | PLUS of expr * expr 
    | MINUS of expr * expr 
    | MULT of expr * expr 
    | DIVIDE of expr * expr 
    | MAX of expr list 
    | VAR of id 
    | LET of id * expr * expr 

  type environment
  type value 

  val emptyEnv : environment 
  val eval : environment * expr -> value 
  val int_of_value : value -> int 
end 

module Zexpr =
  struct 
	exception Error of string 
  type id = string 
  type expr = 
    | NUM of int 
    | PLUS of expr * expr 
    | MINUS of expr * expr 
    | MULT of expr * expr 
    | DIVIDE of expr * expr 
    | MAX of expr list 
    | VAR of id 
    | LET of id * expr * expr 
	type environment = (id * expr) list
  type value  = int
	let emptyEnv = []
  
  let rec eval (env, exp ) =
  	match exp with
  	| NUM n -> n
  	| PLUS (a,b) -> eval (env, a) + eval (env, b)
  	| MINUS (a,b) ->  eval (env, a) - eval (env, b)
  	| MULT (a,b) ->  eval (env, a) * eval (env, b)
  	| DIVIDE (a,b) ->  eval (env, a) / eval (env, b)
    | VAR var -> find_val (env, var) 
  	| LET (var, valu, eq) ->  let v = eval (env, valu)in  eval( (var , NUM v) :: env, eq) 
    | MAX li ->     
  		match li with
  		| [] -> 0
  		| _ -> find_max (env, li, min_int)

  and find_val (env, x) =
  	match env with
  	| [] -> raise (Error "FreeVariable")
  	| el::env' -> 
  		match el with
  		| (var, valu) -> if( var = x ) then eval(env, valu) else find_val ( env' , x) 
			| _ -> raise (Error "Something wrong")

	and find_max (env, li, max) =
  	match li with
  	| [] -> max
  	| el::li' -> let new_el = eval(env, el) in find_max (env,  li', (if(new_el > max ) then  new_el else max))

   let int_of_value v =
  	match v with
  	| n-> n
  	| _ -> raise (Error "eval is wrong")

end

(*	module ValidZexpr = (Zexpr: ZEXPR)*)
	
(* 0) 문법을 ocaml에 맞게 조금 수정한 것입니다. 
1) 뼈대 코드수정된 모듈 타입(시그니처)에는 구현해야 할 함수가 하나 더 추가되어 있습니다. (int_of_value) 
2) 숙제 문서에 있는 "최종 값을 프린트하고..."는 위의 int_of_value 함수를 구현하는 것으로 대체합니다. 

- 아래 각각의 에러 상황에 대해서, 다음과 같은 문자열을 인자로 하는 Error 예외를 발생시키시면 됩니다. 
1) 현재 환경에서 정의되지 않은 이름이 사용될 때 : "FreeVariable" *)

(* vironment, value의 정의는, 
1) 코드상에서는 사실 원하는대로 정하시고 구현하셔도 됩니다. signature를 만족하는 모듈을 어떻게든 정의하면 된다는 뜻입니다. (하지만, 아래 3) 내용을 꼭 읽어주세요.) 
2) 의미상의 정의는 (진도가 나갔는지 모르겠지만) 교수님의 강의 슬라이드 3.ppt 의 25 페이지부터 참고하시면 되겠습니다. 물론 채점은 의미상 정의에 따라 이루어집니다. 
3) 1)에서 모든 수강생들이 자신만의 모듈을 구현한다면 조교팀에서 채점하기 매우 힘들어지겠죠 Sad 
따라서 숙제 2-4의 경우 뼈대 코드를 조교팀에서 제공할 예정입니다. 
금주 중으로 뼈대 코드를 업로드할 예정이니 다른 숙제를 먼저 해결해 주세요 Smile 

인용:
그리고 예시로 들어주신 문장에서 숫자 1을 그냥 사용하셧는데 정의에 따르면 NUM 1로 수정되어야 하지 않을까 생각됩니다.

맞습니다. NUM 을 붙여 생각하시면 됩니다. 
(변경 스펙 게시물에 추가)*)