let sigma (tuple : int * int * (int -> int)) : int =
  let (a,b,f) = tuple in
  if (a > b) then 0
  else (
    let sum = ref 0 in
    for n = a to b do
      sum := f(n) + !sum
    done;
    !sum
  )

(*
let test (f : 'a -> 'b) (input : 'a) (output : 'b) : unit =
  if ((f input) = output)
    then ((print_string ("correct answer")); (print_newline ()))
    else ((print_string ("wrong answer")); (print_newline ()))

let _ =  
  let test_sigma = test sigma in
  (test_sigma (1,10,(fun x -> x + 1)) 65);
  (test_sigma (10,1,(fun x -> x + 1)) 0)
*)
