ocamlc -c -I .. Util.ml || exit 1
ocamlc -c -I .. Quantiles.ml || exit 2
ocamlc -o testUtil01 -I .. nums.cma Streaming.cmo Util.cmo testUtil01.ml || exit 3
ocamlc -o testQuantiles02 -I .. nums.cma Streaming.cmo Util.cmo Quantiles.cmo testQuantiles02.ml || exit 4
ocamlc -o testQuantiles03 -I .. nums.cma Streaming.cmo Util.cmo Quantiles.cmo testQuantiles03.ml || exit 5
ocamlc -o testQuantiles04 -I .. nums.cma Streaming.cmo Util.cmo Quantiles.cmo testQuantiles04.ml || exit 6

#rlwrap ocaml -I .. nums.cma Streaming.cmo Util.cmo Quantiles.cmo

# Copyright 2016 David Felber.
