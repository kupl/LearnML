let rec sigma : int * int * (int -> int) -> int = fun (a, b, f) ->
  if a>b then 0
  else f a + sigma (a+1, b, f)

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
