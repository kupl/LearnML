(*
Name: Yoon Jae Nam (2012-81338)
Organization: Seoul National University
Class: Programming Language (4190.310)
Problem: 4: sum prod
*)

(* rowProd: Gets the product of the elems in the nth row. 
Precondition: n >= 1 *)
let rec rowProd : (int * int -> float) * int * int -> float = 
	fun (matrix, n, k) ->
		match k with
		0 -> 1.0 (* Base case *)
		| _ -> 
		let elem : float = matrix(n, k) in (* Cell elem *)
		let prodOneColLess : float = rowProd(matrix, n, k - 1) in
			elem *. prodOneColLess
(* End of rowProd definition *)

(* sumprod function *)
let rec sumprod : (int * int -> float) * int * int -> float = 
	fun (matrix, n, k) ->
		match (n, k) with
		(0, _) -> 0.0 (* Base case: no rows *)
		| (_, _) -> (* At least one row exists *)	
			let prevSumProd : float = sumprod(matrix, n - 1, k) in
			let currRowProd : float = rowProd(matrix, n, k) in
				prevSumProd +. currRowProd (* result of sumprod*)
(* End of sumprod*)		
(*
(* Below: test code *)

(* For testing.
Prints a pair of int in the following format: (x, y) *)
let printIntPair = fun (i, j) ->
	print_string "(";
	print_int i;
	print_string ", ";
	print_int j;
	print_string ")";
	print_newline ()
	
(*
For testing.
*)
let test_runner = fun (test_name, matrix_data, n, k, expected) ->
	let test_matrix : (int * int) -> float = fun (i, j) ->
		List.nth (List.nth matrix_data (i - 1)) (j - 1) in
	let actual = sumprod(test_matrix, n, k) in
		print_endline ("---------------");
		if actual = expected then
			print_endline ("Good (" ^ test_name ^ ")")
		else
			print_endline ("***BAD (" ^ test_name ^ ")");
			print_string "Expected: ";
			print_float expected;
			print_newline ();
			print_string "Actual: ";
			print_float actual;
			print_newline ()
			
let test1 =
	let matrix_data = [[1.5; 2.0]; [3.0; 4.0]] in
	let n = 2 in
	let k = 2 in
	let expected = 15.0 in
		test_runner("test1", matrix_data, n, k, expected)
		
let test1_1 =
	let matrix_data = [[1.5; 2.0]; [3.0; 4.0]] in
	let n = 1 in
	let k = 2 in
	let expected = 3. in
		test_runner("test1_1", matrix_data, n, k, expected)
		
let test1_2 =
	let matrix_data = [[1.5; 2.0]; [3.0; 4.0]] in
	let n = 2 in
	let k = 1 in
	let expected = 4.5 in
		test_runner("test1_2", matrix_data, n, k, expected)

let test2 =
	let matrix_data = [[1.0]; [15.5]] in
	let n = 2 in
	let k = 1 in
	let expected = 16.5 in
		test_runner("test2", matrix_data, n, k, expected)
		
let test3 =
	let matrix_data = [[1.0; 2.0; 3.0]; [4.0; 5.0; 6.0]] in
	let n = 2 in
	let k = 2 in
	let expected = 22.0 in
		test_runner("test3", matrix_data, n, k, expected)
		
let test4 =
	let matrix_data = [[1.0; 2.0; 3.0]; [4.0; 5.0; 6.0]; [1.5; 1.5; 100.0]] in
	let n = 3 in
	let k = 3 in
	let expected = 351.0 in
		test_runner("test4", matrix_data, n, k, expected)
*)