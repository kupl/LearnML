let rec fib
= fun n -> match n with
| 0 -> 0
| 1 -> 1
| n -> fib(n-2) + fib(n-1)

