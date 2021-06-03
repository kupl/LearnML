# Installation

We provide a full VirtualBox image to install LearnML. This image contains all benchmarks, dependencies, and a data-driven feedback generator, modeling version of [SARFGEN](https://dl.acm.org/doi/10.1145/3192366.3192384), for comparison as well as our framework, LearnML. 

## Installing a Virtual Machine Image

1. Download and install Oracle VM VirtualBox at [here](https://www.virtualbox.org/wiki/Downloads)
2. Download the VM image: [FSE21_CAFE_artifacts.tar.gz](TODO) 
3. Install the `.vdi` file with VirtualBox.

*NOTE:*

- The size of the VM image: The size is about **TODO**. When decompressing it, you obtain about **TODO** `.vdi` file.
- **The experiement setting: In our paper, we conducted all experiments on n an iMac with Intel i5 CPU and 16GB memory.** 
Therefore, when running LearnML by using the virtualbox with the different settings, it may differ from the experimental results on our paper. We recommand to run LearnML with **the VM image having 8GB memory and four virtual processors**.
