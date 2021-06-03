# LearnML

LearnML is a framework for providing personalized feedbacks on functional programming assignments written by OCaml. Our framework consists of the following three tools:

* **FixML** ([OOPSLA_18](https://dl.acm.org/doi/10.1145/3276528)) : **search-based** automated feedback generator
* **TestML** ([OOPSLA_19](https://dl.acm.org/doi/10.1145/3360614)): logical error detector using **counter-example generation**
* **CAFE** ([FSE_21](TODO)): **context-aware** and **data-driven** feedback generator

All the tools are licensed under the [MIT license](LICENSE.txt).

## Installation
We provide a simple way to install LearnML:
* A VirtualBox image containing all resources to reproduce the main results (Table 1 and Figure 6) of our paper: [FSE21_CAFE_artifacts.tar.gz](TODO)
   * Ubuntu ID/PW: CAFE/CAFE
   
Please see [INSTALL.md](./INSTALL.md) for full installation instructions.

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
