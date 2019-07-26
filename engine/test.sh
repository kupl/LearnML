#!/bin/bash

#Similar with reference solution
#./main.native -submission ../benchmarks/KoreaUniv_wellformedness/15/sub32.ml -solution ../benchmarks/KoreaUniv_wellformedness/sol.ml

#Motivating example 1
#./main.native -submission ../incorrect_fail.ml -solution ../correct_metro.ml

#Issue1: cannot extract summary precisely
#./main.native -tree -submission ../benchmarks/KoreaUniv_wellformedness/15/sub68.ml

#Issue2: matching error(return matching true between different summary)
#./main.native -submission ../benchmarks/Test_diff/15/sub7.ml -solution ../benchmarks/Test_diff/17/sub97.ml
#./main.native -submission ../benchmarks/Test_diff/15/sub7_test.ml -solution ../benchmarks/Test_diff/17/sub97.ml


#matching testing
#./main.native -fix -submission ../incorrect_fail.ml -solutions ../benchmarks/KoreaUniv_wellformedness/
#./main.native -fix -submission ../incorrect_fail.ml -solutions ../benchmarks/Test_wellformedness/
#./main.native -fix -submission ../benchmarks/Test_wellformedness/16/sub127.ml -solutions ../benchmarks/Test_wellformedness/
#./main.native -fix -submission ../benchmarks/Test_wellformedness/16/sub43.ml -solutions ../benchmarks/Test_wellformedness/


#interesting matching
#./main.native -submission ../benchmarks/Test_diff/15/sub21.ml -solution ../benchmarks/Test_diff/16/sub26.ml

#Vectorization Test

#./main.native -tree -submission ../benchmarks/KoreaUniv_wellformedness/15/sub68.ml
#./main.native -vector -submission ../benchmarks/KoreaUniv_wellformedness/15/sub68.ml

#Issue1: Not found(DONE!)
#./main.native -run -submission ../benchmarks/KoreaUniv_diff/15/sub12.ml -testcases ../benchmarks/KoreaUniv_diff/testcases -entry diff
#wrong command! diff's entry func is grading

#Data driven
./main.native -submission ../benchmarks_incorrect/KoreaUniv_diff/18/sub8.ml -solutions ../benchmarks_correct/KoreaUniv_diff/ -testcases ../benchmarks_correct/KoreaUniv_diff/testcases
