type metro = STATION of name
    | AREA of name * metro
    | CONNECT of metro * metro
and name = string
;;

let rec checkWithTwoVariables (regions, stations) = 
    match stations with
     AREA(reg, rest) -> checkWithTwoVariables ((reg::regions), rest) (*region 추가, 나머지 metro와 match*)
    | CONNECT(rest1, rest2) -> checkWithTwoVariables (regions, rest1) && checkWithTwoVariables (regions, rest2)(*각 부분을 지금까지 얻은 region과 비교*)
    | STATION stat -> (*station이 지금까지의 region list에 있나 판별*)
    if List.mem stat regions then true
    else false
;;

let rec checkMetro(m : metro) : bool = 
    (* 변수 하나로 이 함수를 구현하기 매우 힘들다. 지역과 역 이름의 두 변수를 비교하는 것이 좋을 것이다.*)
    match m with
     AREA (region, restMetro) -> checkWithTwoVariables (region::[], restMetro)
    | CONNECT(metro1, metro2) -> checkMetro(metro1) && checkMetro(metro2)
    | _ -> false
;;

