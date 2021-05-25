(* Mechanical & Aerospace Eng./2013-11706/Kang Injae/1-2.ml *)

let rec sigma ((a : int), (b : int), (f : 'n -> 'fn)) : int =
  if a > b then 0
  else (f a) + (sigma (a+1, b, f))

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
    test_sigma 1 10 (fun x -> 2 * x) 110;
    test_sigma 1 10 (fun x -> x * x) 385;
    test_sigma 3 1 (fun x -> x * x) 0;
    test_sigma 3 3 (fun x -> x * x * x) 27;
    test_sigma (-10) (-1) (fun x -> x * x) 385;
    test_sigma 11 10 (fun x -> 2 * x) 0;
*)
