# GRC COMPILER -- GRCC

GRCC is a compiler for the Grace programming language. It's intended to support all Grace language features, specified in
https://din-ilp2018.readthedocs.io/en/latest/.

## Building it

```
mkdir build && cd build
cmake ..
make
```

## Usage

```
Usage: grcc [options] -f <filename>
Options:
  -h           Display this information.
  -O           Active optimization.
  -l           Write scopes in file 'GRCLog.out'.
  -v           Display IR.
```
