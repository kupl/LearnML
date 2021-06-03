# LearnML

LearnML is a framework for providing personalized feedbacks on functional programming assignments written by OCaml. Our framework consists of the following three tools:

* **FixML** ([OOPSLA_18](https://dl.acm.org/doi/10.1145/3276528)) : **search-based** automated feedback generator
* **TestML** ([OOPSLA_19](https://dl.acm.org/doi/10.1145/3360614)): logical error detector using **counter-example generation**
* **CAFE** (FSE_21): **context-aware** and **data-driven** feedback generator

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
