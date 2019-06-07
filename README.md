# scheme2c
Scheme to C compiler written in Scheme and C

Summary:

        Macroexpand                    Compile                    CCompile                 Wrap                 Assemble
Scheme -------------> simpler scheme  ---------> register scheme ----------> C (fragment) ------> C (complete) ----------> Executable
                                          A                                                  A
                                          |                                                  |
                                          |<-------------------- Data -----------------------/
                                          |
                                          V
                                   Lexical addressing
                                   Open-Code primitives
                                   Needed functions
                                   Etc.
TODO:
- metacircular evaluator
  - more c primitives
  - create scheme library
- speed and memory
  - optimizations
- quasiquote for sicp-compiler?
- garbage collecter


A work in progress.

Check design.txt for more details.
