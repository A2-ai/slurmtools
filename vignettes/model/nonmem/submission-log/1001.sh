#!/bin/bash
#SBATCH --job-name="1001-nonmem-run"
#SBATCH --nodes=1
#SBATCH --ntasks=1
#SBATCH --cpus-per-task=1
#SBATCH --partition=cpu2mem4gb

unset SLURM_NODELIST
unset SLURM_JOB_NODELIST
unset SLURM_JOB_USER
unset SLURM_TASKS_PER_NODE
unset SLURM_JOB_UID
unset SLURM_TASK_PID
unset SLURM_LOCALID
unset SLURM_SUBMIT_DIR
unset SLURMD_NODENAME
unset SLURM_NODE_ALIASES
unset SLURM_CLUSTER_NAME
unset SLURM_CPUS_ON_NODE
unset SLURM_JOB_CPUS_PER_NODE
unset SLURM_GTIDS
unset SLURM_JOB_PARTITION
unset SLURM_JOB_NUM_NODES
unset SLURM_JOBID
unset SLURM_PROCID
unset SLURM_TOPOLOGY_ADDR
unset SLURM_TOPOLOGY_ADDR_PATTERN
unset SLURM_WORKING_CLUSTER
unset SLURM_NODELIST
unset SLURM_PRIO_PROCESS
unset SLURM_NNODES
unset SLURM_SUBMIT_HOST
unset SLURM_JOB_ID
unset SLURM_NODEID
unset SLURM_CONF
unset SLURM_JOB_NAME
unset SLURM_JOB_GID
unset SLURM_JOB_NODELIST

# submit_nonmem_model uses the whisker package to populate template files
# https://github.com/edwindj/whisker


/usr/local/bin/bbi nonmem run local /cluster-data/user-homes/wes/packages/slurmtools/vignettes/model/nonmem/1001.mod --config /cluster-data/user-homes/wes/packages/slurmtools/vignettes/model/nonmem/bbi.yaml
