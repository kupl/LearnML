let sigma f a b =
let result = ref 0 in
for i = a to b do
result := !result + f i
done;
!result;;
