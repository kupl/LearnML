type nat = ZERO 
         | SUCC of nat

let rec natadd ((a : nat),(b : nat)) : nat =
match a with
  |ZERO -> b
  |SUCC atl -> natadd (atl, (SUCC b))


let rec natmul (a, b) =
  match a with
    |ZERO -> ZERO
    |SUCC atl -> natadd(natmul(atl,b),b)


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

      open Nat
      let _ =
        let str_conv = string_of_int in
      
        let rec nat_to_int =
          fun n ->
            match n with
            | ZERO -> 0
            | SUCC n1 -> 1 + nat_to_int n1
        in
        let three = SUCC (SUCC (SUCC ZERO)) in
        let four = SUCC three in
        let five = SUCC four in
      
        print_test_equal ~test_name:("nat1") str_conv 7 (nat_to_int (natadd (three, four)));
        print_test_equal ~test_name:("nat2") str_conv 0 (nat_to_int (natadd (ZERO, ZERO)));
        print_test_equal ~test_name:("nat3") str_conv 3 (nat_to_int (natadd (ZERO, three)));
        print_test_equal ~test_name:("nat4") str_conv 4 (nat_to_int (natadd (four, ZERO)));
      
        print_test_equal ~test_name:("nat5") str_conv 15 (nat_to_int (natmul (three, five)));
        print_test_equal ~test_name:("nat6") str_conv 12 (nat_to_int (natmul (three, four)));
        print_test_equal ~test_name:("nat7") str_conv 0 (nat_to_int (natmul (ZERO, three)));
        print_test_equal ~test_name:("nat8") str_conv 0 (nat_to_int (natmul (four, ZERO)));
        print_test_equal ~test_name:("nat9") str_conv 0 (nat_to_int (natmul (ZERO, ZERO)));
        print_test_equal ~test_name:("nat10") str_conv 3 (nat_to_int (natmul (SUCC ZERO, three)));
        print_test_equal ~test_name:("nat11") str_conv 4 (nat_to_int (natmul (four, SUCC ZERO)));
*)