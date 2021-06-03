# LearnML

LearnML is a framework for providing personalized feedbacks on functional programming assignments written by OCaml. Our framework consists of the following three tools:

* **FixML** ([OOPSLA_18](papers/OOPSLA_18.pdf)) : **search-based** automated feedback generator
* **TestML** ([OOPSLA_19](papers/OOPSLA_19.pdf)): logical error detector using **counter-example generation**
* **CAFE** ([FSE_21](papers/FSE_21.pdf)): **context-aware** and **data-driven** feedback generator

All the tools are licensed under the [MIT license](LICENSE.txt).

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
