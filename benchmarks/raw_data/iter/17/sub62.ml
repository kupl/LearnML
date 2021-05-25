let rec iter ((n : int), (f : 'a -> 'a)): 'a -> 'a =
  if n = 0
  then (fun x -> x)
  else
  if n = 1
  then f
  else fun x -> f (iter(n-1,f) x)

  

(*
let print_test test_name str_conv cmp answer result =
  let compare_result = cmp answer result in
  match test_name, compare_result with
  | "", compare_result -> (
      match compare_result with
      | true -> print_endline (
          string_of_bool compare_result
        )
      | false -> print_endline (
          (string_of_bool compare_result) ^
          " answer: " ^ (str_conv answer) ^
          " result: " ^ (str_conv result)
        )
    )
  | test_name, compare_result -> (
      match compare_result with
      | true -> print_endline (
          test_name ^ ": " ^ (string_of_bool compare_result)
        )
      | false -> print_endline (
          test_name ^ ": " ^
          (string_of_bool compare_result) ^
          " answer: " ^ (str_conv answer) ^
          " result: " ^ (str_conv result)
        )
    )

let print_test_equal ?test_name:(test_name="") str_conv answer result =
  print_test test_name str_conv (=) answer result

let _ =
  let str_conv = string_of_int in
  print_test_equal ~test_name:("iter1") str_conv 10 (iter (5, function x -> 2 + x) 0);
  print_test_equal ~test_name:("iter2") str_conv 32 (iter (5, function x -> 2 * x) 1);
  print_test_equal ~test_name:("iter3") str_conv 3 (iter (0, function x -> 2 * x) 3);
  *)