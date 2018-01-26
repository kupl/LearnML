let rec prime n =
 let rec sub_prime (i,j)
  if j==1 then true
  else if (i mod j) == 0 then false
  else sub_prime(i,j-1) in
  sub_prime(n,int_of_float(sqrt(float_of_int(n))));;
