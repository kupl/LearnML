(*컴퓨터공학부 2014-16775 김민지
programming language hw 1-1*)

let rec merge((x:int list), (y:int list)): int list =
  match (x, y) with
  |([], _) -> y
  |(_, []) -> x
  |(hx::tx, hy::ty) -> if hx >= hy then hx::merge(tx, y)
                       else hy::merge(x, ty)
