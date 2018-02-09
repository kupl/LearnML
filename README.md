# LearnML
A framework for generating personalized feedbacks on functional programming assignments

## Install Packages
```sh
$ opam install core
$ opam install batteries
$ opam install menhir
$ opam install aez
```

## Running
```sh
$ cd engine
$ ./build
$ ./main.native -execute -submission ../benchmarks/factorial/sub1.ml
$ ./main.native -run -submission ../benchmarks/factorial/sub1.ml -testcases ../benchmarks/factorial/testcases -entry factorial
$ ./main.native -fix -submission ../benchmarks/factorial/sub1.ml -solution ../benchmarks/factorial/sol.ml -testcases ../benchmarks/factorial/testcases -entry factorial
```

## Running Script
```sh
$ python run.py
```
