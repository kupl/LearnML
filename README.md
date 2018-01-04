# LearnML
A framework for generating personalized feedbacks on functional programming assignments

## Install
```sh
$ opam install core
$ opam install batteries
$ opam install menhir
```

## Running
```sh
$ cd engine
$ ./build
$ ./main.native -run -submission ../benchmarks/factorial/sub1.ml -testcases ../benchmarks/factorial/testcases
$ ./main.native -fix -submission ../benchmarks/factorial/sub1.ml -solution ../benchmarks/factorial/sol.ml -testcases ../benchmarks/factorial/testcases
```
