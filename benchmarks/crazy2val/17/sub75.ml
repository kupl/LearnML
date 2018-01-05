type crazy2 = NIL 
| ZERO of crazy2 
| ONE of crazy2 
| MONE of crazy2

let rec crazy2val n =
  match n with
  |NIL -> 0
  |ZERO(m) -> 0 + 2*crazy2val(m)
  |ONE(m) -> 1 + 2*crazy2val(m)
  |MONE(m) -> -1 + 2*crazy2val(m)


  let mtwo = ZERO(ONE(MONE NIL)) 
  let one = ONE(NIL) 
  let five = ONE(ZERO(ONE NIL)) 
  let mone = ONE(MONE NIL) 
  let mnine = ONE(MONE(ZERO(MONE NIL))) 
  let zero = ZERO(ZERO(ZERO NIL)) 
  let big1 = ZERO(ONE(ZERO(ONE(ZERO(MONE(MONE(ONE(MONE(ONE NIL))))))))) 
  let big2 = ONE(MONE(MONE(ZERO(ONE(ZERO(MONE(MONE(ONE(MONE NIL))))))))) 
  
  let _ = 
    let test_case : int * int * int -> unit = fun (n, x, y) -> 
      let result : int * int -> string = fun(x, y) -> 
        if(x == y) then "Pass" 
        else "Failure -> " ^ string_of_int(x) ^ " vs " ^ string_of_int(y) in 
      print_endline ("Case " ^ string_of_int(n) ^ " : " ^ result(x, y)) in 
    test_case(1, -2, crazy2val(mtwo)); 
    test_case(2, 1, crazy2val(one)); 
    test_case(3, 5, crazy2val(five)); 
    test_case(4, -1, crazy2val(mone)); 
    test_case(5, -9, crazy2val(mnine)); 
    test_case(6, 0, crazy2val(zero)); 
    test_case(7, 298, crazy2val(big1)); 
    test_case(8, -437, crazy2val(big2));