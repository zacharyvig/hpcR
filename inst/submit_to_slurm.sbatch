#!/bin/bash
#SBATCH -N 1
#SBATCH -n 4
#SBATCH -t 4:00:00
#SBATCH --mem=4G
#SBATCH -b now+10

set -euo pipefail
IFS=$'\n\t'

export JOB_ID="$SLURM_JOB_ID"
[[ -n "$SLURM_ARRAY_TASK_ID" ]] && export ARRAY_ID="$SLURM_ARRAY_TASK_ID"
cd "$SLURM_SUBMIT_DIR"

[[ -n "$shell_code" ]] && eval "$shell_code"

"${R_HOME}/bin/Rscript" --vanilla "$run_system_file" \
  --print_session_info "$print_session_info" \
  --print_environment "$print_environment" \
  --packages "$packages" \
  #--input_rdata_file "$input_rdata_file" \
  --input "$input" \
  #--is_r_script_tmp "$is_r_script_tmp" \
  #--wait_for_subs "$wait_for_subs" \
  #--repolling_interval "$repolling_interval" \
  #--max_wait "$max_wait" \
  --scheduler_name "$scheduler_name" \
  #--all_subs_success "$all_subs_success" \
  #--post_subs_r_script "$post_subs_r_script" \
  #--is_post_subs_r_script_tmp "$is_post_subs_r_script_tmp" \
  #--output_rdata_file "$output_rdata_file"

