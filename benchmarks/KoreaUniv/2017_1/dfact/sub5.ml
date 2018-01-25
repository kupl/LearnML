(* problem 5*)

let dfact : int -> int
= fun n -> let rec func n = if n=1 || n=2 then n
                                          else n * func (n-2)
  in func n