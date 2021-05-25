(*컴퓨터공학부 2014-16775 김민지
programming language hw 1-3*)

let rec iter ((n: int), f) =
  if n <= 0 then (fun y -> y)
  else (fun y -> (iter (n-1, f) (f y)))
