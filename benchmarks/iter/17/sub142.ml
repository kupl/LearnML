let iter ((n:int), (f: int->int)) (x:int) :int =
  if n<=0 then 0
  else let count = ref (f x) in
    for i = 1 to n-1 do
      count := f !count;
    done;
  !count

(* let a33 = iter (11, function x -> 2*x+1) 7

let _ = print_endline (string_of_int a33) *)
