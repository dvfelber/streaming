# streaming
Some streaming algorithm implementations.



Specifically,

1. A (mergeable) quantile summary with (additive, deterministic) semantics,
   based on
   1. Munro, J. Ian, and Mike S. Paterson. "Selection and sorting with limited
      storage." Theoretical computer science 12.3 (1980): 315-323.
   2. Agarwal, Pankaj K., et al. "Mergeable summaries." ACM Transactions on
      Database Systems (TODS) 38.4 (2013): 26.

2. A quantile summary with (additive, randomized) semantics, based on
   1. Felber, David, and Rafail Ostrovsky. "A Randomized Online Quantile Summary
      in O (1/epsilon* log (1/epsilon)) Words." LIPIcs-Leibniz International
      Proceedings in Informatics. Vol. 40. Schloss Dagstuhl-Leibniz-Zentrum fuer
      Informatik, 2015.



The main purposes in implementing these algorithms were:

1. To have an implementation of the second algorithm that people could see.

2. To try implementing some non-trivial algorithms in OCaml, to learn it.



Some notes about the implementation:

1. This implementation is based on an earlier one that I did in Haskell.

2. The code definitely works for int type items, and should also work for any
   types for which (<) is defined. It probably won't work for other types yet.

3. To compile everything, run "make.sh" in the top-level directory.

4. A description of some implementation choices can be found at tex/notes.pdf
   after compiling the project.

5. The main code files are:

   1. Streaming/Quantiles.ml : this is the (mergeable) quantile summary with
      (additive, deterministic) semantics.

   2. Streaming/Quantiles/Randomized.ml : this is the (mergeable) quantile
      summary with (additive, randomized) semantics.

   3. Streaming/Quantiles/Mediated.ml : this is the (mergeable) quantile summary
      with (additive, randomized) semantics, except that it also accepts a bound
      on the probability that a query might fail.



Copyright 2016 David Felber.
