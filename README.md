# PADL_2021
This is an anonymous repository containing ConFuzz and experimental subjects used in the evaluation of the paper titled *ConFuzz: Coverage-guided Property Fuzzing for Event-driven Programs*

# Dependencies
Following dependencies need to be installed to run ConFuzz:
1. [`opam`](https://opam.ocaml.org/doc/Install.html)
2. `libev` package. It is often called libev-dev or libev-devel
3. [`afl(American Fuzzy Lop)`](https://github.com/google/AFL)

# Set Up
1. At the root of the repository, execute `opam switch install .  ocaml-variants.4.08.0+afl`
2. Add `afl` to environment PATH

# Running benchmarks
Benchmarks are located under `/evaluation_subjects` with `/real_world_programs` and `/benchmarks` as sub-directories as classified in the paper. 
Follow the below steps to execute a benchmark:

1. cd into the benchmark directory i.e. `evaluation_subjects/benchmarks/B1`
1. Each benchmark can be executed under three modes:
   1. To execute under *ConFuzz* run `make`
   1. To execute under *Node.Fz*, run `make run_node_fz`
   1. To execute under *Stress testing*, `make run_stress`
1. To reproduce crash, execute the test binary with crash file under `op/crashes` i.e `./test.out op/crashes/id:000000,sig:06,src:000047,op:havoc,rep:4`
