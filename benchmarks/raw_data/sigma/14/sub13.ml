(* ���α׷��־�� Homework1 2009-11657 �赿�� *)

(* Exercise 1 *)
let sigma (a, b, f) =
  if a > b then 0
  (* ���� ���� �� ������ ū ���� �߸��� �Է����� �����ϰ� 0 return *)
  else
    let rec aux c d =
      if c >= b then d
      else aux (c + 1) (d + (f (c + 1))) in
    aux a (f a)



