#!/bin/sh
set -euo pipefail
IFS=$'\n\t'

export JOBID=$$

[[ -n "${shell_code:-}" ]] && eval "$shell_code"
cd "$PWD"

"${R_HOME}/bin/Rscript" --vanilla "$run_system_file" \
  --print_session_info "${print_session_info:-FALSE}" \
  --print_environment "${print_environment:-FALSE}" \
  --packages "${packages:-}" \
  --input "${input:-}" \
  --scheduler_name "${scheduler_name:-slurm}"
  #--input_rdata_file "$input_rdata_file" \
  #--is_r_script_tmp "$is_r_script_tmp" \
  #--wait_for_subs "$wait_for_subs" \
  #--repolling_interval "$repolling_interval" \
  #--max_wait "$max_wait" \
  #--all_subs_success "$all_subs_success" \
  #--post_subs_r_script "$post_subs_r_script" \
  #--is_post_subs_r_script_tmp "$is_post_subs_r_script_tmp" \
  #--output_rdata_file "$output_rdata_file"
