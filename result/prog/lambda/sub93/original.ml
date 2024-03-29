type lambda = V of var | P of var * lambda | C of lambda * lambda

and var = string

let check : lambda -> bool =
 fun lambda ->
  match lambda with
  | V var -> false
  | P (var1, V var2) -> if var1 = var2 then true else false
  | P (var1, C (V var2, P (var3, V var4))) ->
      if var1 = var4 || var2 = var4 || var3 = var4 then true else false
  | P (var1, P (var2, V var3)) ->
      if var1 = var3 || var2 = var3 then true else false
  | P (var1, P (var2, C (V var3, V var4))) ->
      if (var1 = var3 || var2 = var3) && (var1 = var4 || var2 = var4) then true
      else false
  | C (V var1, V var2) -> false
  | P (var1, P (var2, P (var3, V var4))) ->
      if var1 = var4 || var2 = var4 || var3 = var4 then true else false
