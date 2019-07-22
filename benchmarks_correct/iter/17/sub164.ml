let rec iter (pair : int * ('a -> 'a)) : ('a -> 'a) =
  let (n,f) = pair in
  if n <= 0 then (fun x -> x)
  else (
    let compose f g x = f (g x) in
    compose f (iter(n-1,f))
  )

(*
let test (f : 'a -> 'b) (input : 'a) (output : 'b) : unit =
  if ((f input) = output)
    then ((print_string ("correct answer")); (print_newline ()))
    else ((print_string ("wrong answer")); (print_newline ()))

let _ =
  let fu = iter (10, (fun x -> 2 + x) ) in
  print_int (fu 0); (print_newline())
*)
