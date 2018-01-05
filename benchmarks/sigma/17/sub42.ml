(*
  CSE 2012-11226 Kwak Jin Han
	exercise 2
*)


(*
let a = 0
let b = 10
let f x = x
*)

let rec sigma (a, b, f) =
	if a > b then 0
	else if a = b then f b
	else f a + sigma (a+1, b, f)

(*
let result = sigma (a, b, f)

let _ = print_int result
*)

(*
let _ = 
let print_bool x = 
print_endline (string_of_bool x) in 
print_bool (385 = sigma (1, 10, (fun x -> x * x))); 
print_bool (0 = sigma (3, 1, fun x -> x * x)); 
print_bool (27 = sigma(3, 3, fun x -> x * x * x)); 
print_bool (385 = sigma(-10, -1, fun x -> x * x)) 
*)

(*
 let _ =
 let assert_equal (expected: int) (actual: int) =
 if expected = actual then print_endline "true"
 else Printf.printf "Expected %d but actual %d\n" expected actual
 in
 let test_sigma (a: int) (b: int) (f: int->int) (expected: int) =
 sigma (a,b,f) |> assert_equal expected
 in
 test_sigma 1 2 (fun x -> x * x) 5;
 test_sigma 1 100 (fun x -> x) 5050;
 test_sigma 1 1 (fun x -> x) 1;
 test_sigma 1 0 (fun x -> x) 0;
 test_sigma 101 200 (fun x -> -x) (-15050);
 *)
