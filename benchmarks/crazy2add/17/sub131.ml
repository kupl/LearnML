type crazy2 = NIL | ZERO of crazy2 | ONE of crazy2 | MONE of crazy2

let sep : crazy2 -> crazy2 * int = fun cra ->
  match cra with
    NIL -> (NIL,0)
  | ZERO cra1 -> (cra1, 0)
  | ONE cra1 -> (cra1, 1)
  | MONE cra1 -> (cra1, -1)


let rec add1 : crazy2 -> crazy2 = fun cra ->
  match cra with
    NIL -> ONE NIL
  | ZERO cra1 -> ONE cra1
  | ONE cra1 -> ZERO (add1(cra1))
  | MONE cra1 -> ZERO cra1

let rec minus1 : crazy2 -> crazy2 = fun cra ->
  match cra with
    NIL -> MONE NIL
  | ZERO cra1 -> MONE cra1
  | ONE cra1 -> ZERO cra1
  | MONE cra1 -> ZERO(minus1(cra1))


let rec crazy2add : crazy2 * crazy2 -> crazy2 = fun (cra1, cra2) ->
  match (cra1, cra2) with
    (NIL, cra2) -> cra2
  | (cra1, NIL) -> cra1
  | (cra1, cra2) ->
  (match (sep(cra1), sep(cra2)) with
   | ((scra1,n1),(scra2,n2)) -> if (n1+n2 = 2) then ZERO(crazy2add(scra1,add1(scra2)))
                                 else if(n1+n2 = 1) then ONE(crazy2add(scra1,scra2))
                                 else if(n1+n2=0) then ZERO(crazy2add(scra1,scra2))
                                 else if(n1+n2=(-1)) then MONE(crazy2add(scra1,scra2))
                                 else ZERO(crazy2add(scra1, minus1(scra2)))
  )