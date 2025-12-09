#!/bin/bash
#SBATCH -N 1
#SBATCH -n 4
#SBATCH -t 4:00:00
#SBATCH --mem-per-cpu=4G
#SBATCH -b now+10

export JOB_ID="$SLURM_JOB_ID"
[[ -n "$SLURM_ARRAY_TASK_ID" ]] && export ARRAY_ID="$SLURM_ARRAY_TASK_ID"
cd $SLURM_SUBMIT_DIR

eval $shell_code

"${R_HOME}/bin/Rscript" --vanilla "$compute_file" \
  --print_session_info "$print_session_info" \
  --print_environment "$print_environment" \
  --r_packages "$r_packages" \
  --input_rdata_file "$input_rdata_file" \
  --r_code "$r_code" \
  --wait_for_subs "$wait_for_subs" \
  --repolling_interval "$repolling_interval" \
  --max_wait "$max_wait" \
  --scheduler "$scheduler" \
  --all_subs_success "$all_subs_success" \
  --post_subs_r_code "$post_subs_r_code" \
  --output_rdata_file "$output_rdata_file"

