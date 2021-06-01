type var = string

type lambda = V of var
		   | P of var * lambda
		   | C of lambda * lambda

let rec check_list x l =
  match x with
  | V n -> List.exists (fun m -> (n = m)) l
  | P (n, m) -> check_list m (List.append l [n])
  | C (m1, m2) -> (check_list m1 l) && (check_list m2 l)

let check x = check_list x [] 
