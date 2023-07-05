# SILPA-Intepreter
An interpreter written in OCaml for a simplified but Turing complete version of Pascal called SILPA(Simple Imperative Language with Procedures and Arrays).
Provided you have the OCaml tools installed on your system, you can compile the interpreter as follows:

ocamlfind ocamlc -package core -linkpkg -thread SIL.ml a5.ml frontend.ml -o frontend

The files addarrays.sil, factorial.sil, recursivefactorial.sil and factorize.sil are example of programs that you can write in this SILPA language and that will compile. You can use these as help to create your own programs.
For example here is how to run the factorial and factorize programs:

./frontend factorial.sil 10

--Output--

3628800

./frontend factorize.sil 14

--Output--

2
7
