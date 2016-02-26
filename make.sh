bash make-local.sh || exit 1
(cd tex; bash make.sh) || exit 2
(cd Streaming; bash make.sh) || exit 3
# Copyright 2016 David Felber.
