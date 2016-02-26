# streaming
Some streaming algorithm implementations.



Specifically,

1. a (mergeable) quantile summary with (additive, deterministic) semantics,
   based on
   1. Munro, J. Ian, and Mike S. Paterson. "Selection and sorting with limited
      storage." Theoretical computer science 12.3 (1980): 315-323.
   2. Agarwal, Pankaj K., et al. "Mergeable summaries." ACM Transactions on
      Database Systems (TODS) 38.4 (2013): 26.

2. a quantile summary with (additive, randomized) semantics, based on
   1. Felber, David, and Rafail Ostrovsky. "A Randomized Online Quantile Summary
      in O (1/epsilon* log (1/epsilon)) Words." LIPIcs-Leibniz International
      Proceedings in Informatics. Vol. 40. Schloss Dagstuhl-Leibniz-Zentrum fuer
      Informatik, 2015.



The main purposes in implementing these algorithms were:

1. To have an implementation of the second algorithm that people could see.

2. To try implementing some non-trivial algorithms in OCaml, to learn it.



Some caveats about the implementation:

1. This was based on an earlier implementation in Haskell that I didn't release.
   So, it may not be idiomatic OCaml.

2. The code definitely works for int type items, and should also work for any
   types for which (<) is defined. It probably won't work for other types yet.

3. To compile everything, run "make.sh" in the top-level directory.



Copyright 2016 David Felber.
