# REQUIREMENTS

## Virtual Machine Image

We provide a VirtualBox image. It is based on Ubuntu 20.04 LTS (Focal Fossa) v20210304.0.0.

### Prerequisite

- [VirtualBox](https://www.virtualbox.org/wiki/Downloads)

## From Source

### Setup dependencies

The artifact requires these packages.

| System Package Name | Version |
| :------------------ | :-----: |
| ocaml               | =4.09.0 |
| opam                | ^2.0.5  |

| Opam Package Name |  Version  |
| :---------------- | :-------: |
| Batteries         |  ^3.3.0   |
| Menhir            | ^20210419 |
| OCamlbuild        |  ^0.14.0  |
| OCamlfind         |  ^1.9.1   |
| Z3                |  =4.8.1   |
| Zarith            |   ^1.12   |

### Clone, Build

We are not providing the version build file now.
To use our framework, you have to clone this repository and build it manually.

```bash
$ git clone https://github.com/kupl/LearnML.git
$ cd LearnML/engine
$ ./build
...
```
