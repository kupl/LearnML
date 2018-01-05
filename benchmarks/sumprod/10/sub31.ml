let sumprod (matrix, n, k) = 
 (* ���� �Լ� : int -> int -> (int -> float) -> float *)
 let rec phi pi_start pi_end f_phi =
  if (pi_start = pi_end)
  then (f_phi pi_start)
  else ((f_phi pi_start) *. (phi (pi_start + 1) pi_end f_phi)) 
 in
 
 (* �ñ׸� �Լ� : int -> int -> (int -> float) -> float *)
 let rec sigma si_start si_end f_sig = 
  if (si_start = si_end)
  then (f_sig si_start)
  else ((f_sig si_start) +. (sigma (si_start + 1) si_end f_sig)) 
 in

 (* �ܺ� �Լ� �Է��� matrix�� �� �Լ� ���ο��� �� �� �ְ� ��ȯ : int -> int -> flaot *)
 let matrix_inputDiv i_f j_f = (matrix (i_f, j_f))
 in
 
 (* ���� �ذ��� ���� ���� �Լ� : int -> (int -> float) *)
 let sumprod_sub i =
  (phi 1 k (matrix_inputDiv i))
 in

 (sigma 1 n sumprod_sub);;
