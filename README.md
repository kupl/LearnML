# LearnML

LearnML is a framework for providing personalized feedbacks on functional programming assignments written by OCaml. Our framework consists of the following three tools:

* **FixML** ([OOPSLA_18](https://dl.acm.org/doi/10.1145/3276528)) : **search-based** automated feedback generator
* **TestML** ([OOPSLA_19](https://dl.acm.org/doi/10.1145/3360614)): logical error detector using **counter-example generation**
* **CAFE** ([FSE_21](TODO)): **context-aware** and **data-driven** feedback generator

All the tools are licensed under the [MIT license](LICENSE.txt).

## Installation
We provide a simple way to install LearnML:
* A VirtualBox image containing all resources to reproduce the main results (Table1 and Figure6) of our paper: [FSE21_CAFE_artifacts.tar.gz](TODO)
   * Ubuntu ID/PW: CAFE/CAFE
   
Please see [INSTALL.md](./INSTALL.md) for full installation instructions.

## Artifact information

The VirtualBox image contains the following contents in the directory `~/FSE21_artifacts/`
1. `benchmarks` contains our benchmarks consisting of submissions for 10 programming exercises, TA's golden solution, and test cases with test driver:
    1. `C` contains 3,547 reference solutions which pass test cases.
    2. `I` contains 664 incorrect submissions which cannot pass test cases.
    3. `ta_solutions` contains TA's reference solutions that are used as inputs for FixML and TestML.
    4. `testcases` contains test cases used in evaluation, and testing driver files.
 
2. `engine` contains all several common source codes (e.g., main-driver, language definition, utility functions, etc) and four subdirectories: 
    1. `FixML` contains the implementation of FixML.
    2. `TestML` contains the implementation of TestML.
    3. `Data-driven` contains the implementation of three data-driven approaches (CAFE, fucntion-level SARFGEN, and program-level SARFGEN).
    4. `models` contains the preprocessed data (call-graph) of reference solutions. When new solutions are given to our data-driven feedback generator, it stores the obtained call-graph in here.
    
3. `run.py` is a python script for running all benchmarks by several options.
4. `table.py` is a python script for visualizing evaluation result.
5. `result` contains all results obtained by running `run.py`. Since running the script takes a long time, we also provide the pre-excuted results.

## Reproducing The Results Presented In The Paper
Here we explain how to reproduce our main results (i.e., Table1 and Figure6).

### Table1 and Figure6
1. To reproduce the evaluation results, go to the directory: `~/FSE21_artifacts/` and run the script by: ``` python3 table.py result ``` 
    * Table1 is created from the results of CAFE and FixML
    * Figure6 is created from the resuls of CAFE, fucntion-level SARFGEN, and program-level SARFGEN)
2. If it properly done, you can see the following results:

    **Table1**

    **Figure 6**

### Running the artifact
Since someone may want to run our artifact on their own, we also explain how to run the arfifact and show the result. 
1. Go to the directory: `~/FSE21_artifacts/` and run the script by: ``` python3 run.py ```
    * This script runs all submission with four feedback generators (CAFE, FixML, function-level SARFGEN, and program-level SARFGEN)
    * The results is generated in `result[time]/[tool_name]/[problem]/[submission]`:
      * `original.ml` is original submssion.
      * `patch.ml` is a patch obtained by modifying original.ml. If a system fails to repair, only None is written.
      * `result.txt` contains elapsed time and size of patch (if it calculated).
2. After running `run.py`, type the script: ``` python3 table.py [result_directory] ```. It will show the table and graph according to the evaluation result.

### Testing a specific bernchmark using CAFE
If you want to provide a feedback for a specific benchmark with CAFE, you can run our engine with the following command:

```
engine/main.native -fix -submission [submission_path] -solutoins [solution_dir] -entry [function_name] -testcases [testcase_path] -grading [test_driver](when it exists)
```

For example, you can try to generate a feedback for `benchmarks/I/max/sub1.ml"` by using the following command:

```
engine/main.native -fix -submission benchmarks/I/max/sub1.ml -solutoins benchmarks/C/max/ -entry max -testcases benchmarks/testcases/max_testcases
```

The diff problem (Problem 10 in Table 1) in our benchmark requires a test driver. To generate a feedback for `benchmarks/I/diff/sub1.ml`, run the following script:

```
engine/main.native -fix -submission benchmarks/I/diff/sub1.ml -solutions benchmarks/C/diff/ -entry grading -testcases benchmarks/testcases/diff_testcases -grading benchmarks/testcases/diff_grading.ml
```
