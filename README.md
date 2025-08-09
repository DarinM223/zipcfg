zipcfg
======

Code from the paper 'An Applicative Control-Flow Graph Based on Huet's Zipper' in Standard ML.

Most of the splicing and dataflow code referenced in the paper is buried in the Quick C-- codebase which is abandoned and the files are in noweb which is difficult to read and search for symbols. This repository ports some of the relevant code in the Quick C-- project to Standard ML and simplifies it to have less backend specific details.

The relevant files being in Standard ML makes it less likely to become obselete through language or build system changes and it can make use of well-defined evaluation order in tuples and function parameters unlike OCaml.

To build with MLton, run:

```
mlton zipcfg.mlb
```

To run with SML/NJ, run:

```
sml -m zipcfg.cm
```

To build with PolyML, you need MLton with its libraries installed in a standard location like `/usr/local/lib/mlton`. Run:

```
./build_polyml.sh
poly --use build.sml
```

To build with MLKit, you need MLton with its libraries installed in a standard location like `/usr/local/lib/mlton`. Run:
```
./build_mlkit_smlnjlib.sh
mlkit -o zipcfg zipcfg.mlkit.mlb
```