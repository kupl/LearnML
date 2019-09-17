#!/bin/bash

#Similar with reference solution
#./main.native -submission ../benchmarks/All_lambda/15/sub32.ml -solution ../benchmarks/All_lambda/sol.ml

#Motivating example 1
#./main.native -submission ../incorrect_fail.ml -solution ../correct_metro.ml

#Issue1: cannot extract summary precisely
#./main.native -tree -submission ../benchmarks/All_lambda/15/sub68.ml

#Issue2: matching error(return matching true between different summary)
#./main.native -submission ../benchmarks/Test_diff/15/sub7.ml -solution ../benchmarks/Test_diff/17/sub97.ml
#./main.native -submission ../benchmarks/Test_diff/15/sub7_test.ml -solution ../benchmarks/Test_diff/17/sub97.ml


#matching testing
#./main.native -fix -submission ../incorrect_fail.ml -solutions ../benchmarks/All_lambda/
#./main.native -fix -submission ../incorrect_fail.ml -solutions ../benchmarks/Test_wellformedness/
#./main.native -fix -submission ../benchmarks/Test_wellformedness/16/sub127.ml -solutions ../benchmarks/Test_wellformedness/
#./main.native -fix -submission ../benchmarks/Test_wellformedness/16/sub43.ml -solutions ../benchmarks/Test_wellformedness/


#interesting matching
#./main.native -submission ../benchmarks/Test_diff/15/sub21.ml -solution ../benchmarks/Test_diff/16/sub26.ml

#Vectorization Test

#./main.native -tree -submission ../benchmarks/All_lambda/15/sub68.ml
#./main.native -vector -submission ../benchmarks/All_lambda/15/sub68.ml

#run
./main.native -run -submission ../benchmarks_correct/All_lambda/14/sub9.ml -testcases ../benchmarks_correct/All_lambda/testcases -entry check
#Data driven
./main.native -vector3 -submission ../benchmarks_incorrect/KoreaUniv_diff/18/sub58.ml -solutions ../benchmarks_correct/KoreaUniv_diff/ -testcases ../benchmarks_correct/KoreaUniv_diff/testcases -entry grading -grading ../benchmarks_correct/KoreaUniv_diff/grading.ml
./main.native -vector3 -submission ../benchmarks_incorrect/All_lambda/k_15/sub15.ml -solutions ../benchmarks_correct/All_lambda -testcases ../benchmarks_correct/All_lambda/testcases -entry check
./main.native -vector3 -submission ../benchmarks_incorrect/All_diff/12/sub14.ml -solutions ../benchmarks_correct/All_diff/ -testcases ../benchmarks_correct/All_diff/testcases -entry grading -grading ../benchmarks_correct/All_diff/grading.ml

#Data driven diff example
./main.native -vector3 -submission ../benchmarks_incorrect/All_diff/15/sub41.ml -solutions ../benchmarks_correct/All_diff/ -testcases ../benchmarks_correct/All_diff/testcases -entry grading -grading ../benchmarks_correct/All_diff/grading.ml
