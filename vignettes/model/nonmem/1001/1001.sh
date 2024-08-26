#!/bin/bash

#$ -wd /cluster-data/user-homes/wes/packages/slurmtools/vignettes/model/nonmem/1001

/opt/nonmem/nm751/run/nmfe75 1001.mod  1001.lst  -maxlim=2
