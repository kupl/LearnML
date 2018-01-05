(* 프로그래밍언어 Homework1 2009-11657 김동현 *)

(* Exercise 1 *)
let sigma (a, b, f) =
  if a > b then 0
  (* 시작 값이 끝 값보다 큰 경우는 잘못된 입력으로 간주하고 0 return *)
  else
    let rec aux c d =
      if c >= b then d
      else aux (c + 1) (d + (f (c + 1))) in
    aux a (f a)



