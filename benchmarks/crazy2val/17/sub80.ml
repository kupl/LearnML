(*컴퓨터공학부 2014-16775 김민지
programming language hw 2-2*)

type crazy2 = NIL | ZERO of crazy2 | ONE of crazy2 | MONE of crazy2

let rec crazyhelp (n:int) (y:crazy2) : int =
  match y with
  |NIL -> 0
  |ZERO y1 -> crazyhelp (n+1) y1
  |ONE y1 -> 
    ((2.0**(float_of_int n)) |> int_of_float) + (crazyhelp (n+1) y1)
  |MONE y1 -> 
    (-1 * ((2.0**(float_of_int n)) |> int_of_float)) + (crazyhelp (n+1) y1) 

let crazy2val = crazyhelp 0

