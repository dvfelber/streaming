ocamlc -c -I .. -I ../.. Randomized.ml || exit 1
ocamlc -c -I .. -I ../.. Mediated.ml || exit 2
ocamlc -o testRandomized02 -I .. -I ../.. nums.cma Streaming.cmo Util.cmo Quantiles.cmo Randomized.cmo testRandomized02.ml || exit 3
ocamlc -o testMediated02 -I .. -I ../.. nums.cma Streaming.cmo Util.cmo Quantiles.cmo Randomized.cmo Mediated.cmo testMediated02.ml || exit 4

#rlwrap ocaml -I .. -I ../.. nums.cma Streaming.cmo Util.cmo Quantiles.cmo Randomized.cmo
#rlwrap ocaml -I .. -I ../.. nums.cma Streaming.cmo Util.cmo Quantiles.cmo Randomized.cmo Mediated.cmo

# Copyright 2016 David Felber.
