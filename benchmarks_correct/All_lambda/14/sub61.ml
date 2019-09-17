type lambda = V of var
    | P of var * lambda
    | C of lambda * lambda
and var = string
;;

let rec checkWithTwoVariables (regions, stations) = 
    match stations with
     P(reg, rest) -> checkWithTwoVariables ((reg::regions), rest) (*region 추가, 나머지 lambda와 match*)
    | C(rest1, rest2) -> checkWithTwoVariables (regions, rest1) && checkWithTwoVariables (regions, rest2)(*각 부분을 지금까지 얻은 region과 비교*)
    | V stat -> (*station이 지금까지의 region list에 있나 판별*)
    if List.mem stat regions then true
    else false
;;

let rec check(m : lambda) : bool = 
    (* 변수 하나로 이 함수를 구현하기 매우 힘들다. 지역과 역 이름의 두 변수를 비교하는 것이 좋을 것이다.*)
    match m with
     P (region, restMetro) -> checkWithTwoVariables (region::[], restMetro)
    | C(lambda1, lambda2) -> check(lambda1) && check(lambda2)
    | _ -> false
;;

