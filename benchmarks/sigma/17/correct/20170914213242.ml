let sigma (a, b, f) =
  let sum = ref 0 in
  for i = a to b do
    sum := !sum + (f i)
  done;
  !sum