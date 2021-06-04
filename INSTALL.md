# Installation

We provide a full VirtualBox image to install LearnML. This image contains all benchmarks, dependencies, and a data-driven feedback generator, modeling version of [SARFGEN](https://dl.acm.org/doi/10.1145/3192366.3192384), for comparison as well as our framework, LearnML. 

Please see [REQUIREMENTS.md](./REQUIREMENTS.md) for requirements to installation.

## Installing a Virtual Machine Image

1. Download and install Oracle VM VirtualBox at [here](https://www.virtualbox.org/wiki/Downloads)
2. Download the VM image: [FSE21_CAFE_artifacts.tar.gz](https://drive.google.com/file/d/1JRLXfOugQd7al5jUwkTpWH4pIWF0Bowh/view?usp=sharing) 
3. Install the `.vdi` file with VirtualBox.
   * Ubuntu ID/PW: cafe/cafe
*NOTE:*

- The size of the VM image: The size is about 4.3GB. When decompressing it, you obtain about 20GB `.vdi` file.
- **The experiement setting: In our paper, we conducted all experiments on n an iMac with Intel i5 CPU and 16GB memory.** 
Therefore, when running LearnML by using the virtualbox with the different settings, it may differ from the experimental results on our paper. We recommand to run LearnML with **the VM image having 8GB memory and four virtual processors**.


### Running a bernchmark using CAFE
If you want to provide a feedback for a specific benchmark with CAFE, build our engine by:```cd ~/FSE21_artifacts/engine; ./build```.
After finshing the compilation you can run our engine with the following command:

```
engine/main.native -fix -submission [submission_path] -solutions [solution_dir] -entry [function_name] -testcases [testcase_path] -grading [test_driver](when it exists)
```

For example, you can try to generate a feedback for `benchmarks/I/max/sub1.ml` by using the following command:

```
engine/main.native -fix -submission benchmarks/I/max/sub1.ml -solutions benchmarks/C/max/ -entry max -testcases benchmarks/testcases/max_testcases
```

The diff problem (Problem 10 in Table 1) in our benchmark requires a test driver. To generate a feedback for `benchmarks/I/diff/sub1.ml`, run the following script:

```
engine/main.native -fix -submission benchmarks/I/diff/sub1.ml -solutions benchmarks/C/diff/ -entry grading -testcases benchmarks/testcases/diff_testcases -grading benchmarks/testcases/diff_grading.ml
```
