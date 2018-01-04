type crazy2 = NIL | ZERO of crazy2 | ONE of crazy2 | MONE of crazy2

let crazy2add ((c1 : crazy2), (c2:crazy2)) : crazy2 =
  let zero = 0 in
  let one = 1 in
  let rec carryadder ((c1 : crazy2), (c2 : crazy2), (carry : int)) : crazy2 =
    match c1 with
    | NIL ->
    if (carry == one) then carryadder(ONE(NIL), c2, 0)
    else  if (carry == zero) then c2
    else carryadder(MONE(NIL), c2, 0)
    | ONE(d1) -> (match (c2) with
        | ONE(d2) -> 
          if (carry == one) then ONE(carryadder(d1, d2, 1))
          else  if (carry == zero) then ZERO(carryadder(d1, d2, 1))
          else ONE(carryadder(d1, d2, 0))

        | ZERO(d2) -> 
          if (carry == one) then ZERO(carryadder(d1, d2, 1))
          else  if (carry == zero) then ONE(carryadder(d1, d2, 0))
          else ZERO(carryadder(d1, d2, 0))

        | MONE(d2) -> 
          if (carry == one) then ONE(carryadder(d1, d2, 0))
          else if (carry == zero) then ZERO(carryadder(d1, d2, 0))
          else MONE(carryadder(d1, d2, 0))

        | NIL -> 
        if (carry == one) then carryadder(ONE(NIL), c1, 0)
        else  if (carry == zero) then c1
        else carryadder(MONE(NIL), c1, 0)
      )
    | ZERO(d1) -> (match (c2) with
        | ONE(d2) -> 
          if (carry == one) then ZERO(carryadder(d1, d2, 1))
          else  if (carry == zero) then ONE(carryadder(d1, d2, 0))
          else ZERO(carryadder(d1, d2, 0))

        | ZERO(d2) -> 
          if (carry == one) then ONE(carryadder(d1, d2, 0))
          else if (carry == zero) then ZERO(carryadder(d1, d2, 0))
          else MONE(carryadder(d1, d2, 0))

        | MONE(d2) -> 
          if (carry == one) then ZERO(carryadder(d1, d2, 0))
          else if (carry == zero) then MONE(carryadder(d1, d2, 0))
          else ZERO(carryadder(d1, d2, -1))

          | NIL -> 
          if (carry == one) then carryadder(ONE(NIL), c1, 0)
          else  if (carry == zero) then c1
          else carryadder(MONE(NIL), c1, 0)
      )
    | MONE(d1) -> (match (c2) with
        | ONE(d2) -> 
          if (carry == one) then ONE(carryadder(d1, d2, 0))
          else if (carry == zero) then ZERO(carryadder(d1, d2, 0))
          else MONE(carryadder(d1, d2, 0))

        | ZERO(d2) -> 
          if (carry == one) then ZERO(carryadder(d1, d2, 0))
          else if (carry == zero) then MONE(carryadder(d1, d2, 0))
          else ZERO(carryadder(d1, d2, -1))

        | MONE(d2) -> 
          if (carry == one) then MONE(carryadder(d1, d2, 0))
          else if (carry == zero) then ZERO(carryadder(d1, d2, -1))
          else MONE(carryadder(d1, d2, -1))

          | NIL -> 
          if (carry == one) then carryadder(ONE(NIL), c1, 0)
          else  if (carry == zero) then c1
          else carryadder(MONE(NIL), c1, 0)
      )
  in
  carryadder(c2, c1, 0)



let rec crazy2val(c : crazy2) : int =
  match c with
  | NIL -> 0
  | ZERO(c1) -> 2 * crazy2val(c1)
  | ONE(c1) -> 2 * crazy2val(c1) + 1
  | MONE(c1) -> 2 * crazy2val(c1) - 1

  let mtwo = ZERO(ONE(MONE NIL))
  let one = ONE(NIL)
  let five = ONE(ZERO(ONE NIL))
  let mone = ONE(MONE NIL)
  let mnine = ONE(MONE(ZERO(MONE NIL)))
  let zero = ZERO(ZERO(ZERO(ZERO(ZERO(ZERO(ZERO(ZERO NIL)))))))
  let five2 = ONE(ZERO(ONE(ZERO(ZERO(ZERO(ZERO(ZERO NIL)))))))
  let big1 = ZERO(ONE(ZERO(ONE(ZERO(MONE(MONE(ONE(MONE(ONE NIL)))))))))
  let big2 = ONE(MONE(MONE(ZERO(ONE(ZERO(MONE(MONE(ONE(MONE NIL)))))))))
  
  let _ =
    let test_case : int * int * int -> unit = fun (n, x, y) ->
      let result : int * int -> string = fun(x, y) ->
        if(x == y) then "Pass"
        else "Failure -> " ^ string_of_int(x) ^ " vs " ^ string_of_int(y) in
      print_endline ("Case " ^ string_of_int(n) ^ " : " ^ result(x, y)) in
    let test_nocrazy2val : bool -> unit = fun x ->
      let detector = fun x -> if(x = true) then "Pass" else "crazy2val detected" in
      print_endline ("crazy2val detector : " ^ detector(x)) in
    test_nocrazy2val (crazy2add(zero, five) = five2);
    test_case (1, crazy2val(
      crazy2add(mnine, mtwo)),
       crazy2val(
         crazy2add(mnine, mtwo)));
    test_case (2, crazy2val(mtwo) + crazy2val(mnine), crazy2val(crazy2add(mtwo, mnine)));
    test_case (3, crazy2val(five) + crazy2val(mnine), crazy2val(crazy2add(five, mnine)));
    test_case (4, crazy2val(crazy2add(mnine, one)) + crazy2val(five), crazy2val(mnine) + crazy2val(crazy2add(one, five)));
    test_case (5, crazy2val(crazy2add(crazy2add(mnine, mtwo), crazy2add(five, mtwo))), crazy2val(mnine) + crazy2val(mtwo) + crazy2val(five) + crazy2val(mtwo));
    test_case (6, crazy2val(crazy2add(zero, mnine)), crazy2val(mnine));
    test_case (7, crazy2val(crazy2add(big1, big2)), crazy2val(crazy2add(big2, big1)));
    test_case (8, crazy2val(crazy2add(big1, crazy2add(big1, big2))), crazy2val(crazy2add(crazy2add(big1, big1), big2)));