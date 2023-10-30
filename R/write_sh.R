#' Write job submission script (slurm or PBSPro)
#'
#' Write a submission script to run an array of jobs on a high-performance 
#' computing platform. This function abstracts away specifying the computational
#' requirements of the job, loading the relevant conda environment, and then
#' reading the arguments from the grid file and passing them into an R or python
#' 'inner' file. 
#'
#' @param job_name name of the job that will appear in the job management system
#' @param sh_file path to write the submission script to
#' @param grid_file path to the tab-delimited grid file
#' @param inner_file path to the R/python inner file
#' @param env path to the conda environment
#' @param time time of the job, in hours
#' @param mem memory required for the job, in GB
#' @param cpus CPUs required for the job
#' @param gpu if \code{TRUE}, a GPU will be requested
#'
#' @return returns nothing, but writes the slurm/PBSPro script to the specified
#'   filepath
#'
#' @importFrom purrr map_chr
#' @importFrom magrittr `%>%`
#' @importFrom utils read.delim
#'
#' @export
write_sh = function(job_name,
                    sh_file,
                    grid_file,
                    inner_file,
                    env = 'decoy-generation/env',
                    time = 24, ## in hours
                    mem = 4, ## in GB
                    cpus = 1,
                    gpu = FALSE
) {
  detect_system()

  # read the grid
  grid = read.delim(grid_file)

  # set up the script
  if (current_system == 'sockeye') {
    log_dir = file.path(dirname(base_dir), 'logs', basename(base_dir))
    header_lines = c(
      '#!/bin/bash',
      paste0('#PBS -l walltime=', time, ':00:00,select=1:n', 
             ifelse(gpu, 'gpus=', 'cpus='), cpus,
             ':mem=', mem, 'gb'),
      paste0('#PBS -N ', job_name),
      paste0('#PBS -o ', log_dir, '/', job_name, '-^array_index^.out'),
      paste0('#PBS -e ', log_dir, '/', job_name, '-^array_index^.out'),
      ''
    )

    user = system("echo $USER", intern = TRUE)
    env_lines = c(
      '# >>> conda initialize >>>',
      '# !! Contents within this block are managed by \'conda init\' !!',
      paste0('__conda_setup="$(\'/home/', user,
             '/miniconda3/bin/conda\' \'shell.bash\' \'hook\' 2> /dev/null)"'),
      'if [ $? -eq 0 ]; then',
      'eval "$__conda_setup"',
      'else',
      paste0('if [ -f "/home/', user,
             '/miniconda3/etc/profile.d/conda.sh" ]; then'),
      paste0('. "/home/', user, '/miniconda3/etc/profile.d/conda.sh"'),
      'else',
        paste0('export PATH="/home/', user, '/miniconda3/bin:$PATH"'),
      'fi',
      'fi',
      'unset __conda_setup',
      '# <<< conda initialize <<<',
      '',
      ifelse(grepl('^\\/', env),
             paste0('conda activate ', env),
             paste0('conda activate ', file.path(base_dir, env))
      ),
      ''
    )
  } else if (current_system == 'lsi') {
    log_dir = file.path(dirname(base_dir), 'logs', basename(base_dir))
    header_lines = c(
      '#!/bin/bash',
      paste0('#SBATCH --time=', time, ':00:00'),
      paste0('#SBATCH --job-name=', job_name),
      paste0('#SBATCH --output=', log_dir, '/%x-%j-%a.out'),
      paste0('#SBATCH --mem=', mem, 'G'),
      paste0('#SBATCH --cpus-per-task=', cpus),
      ifelse(gpu, paste0('#SBATCH --gres=gpu:1'), ''),
      ''
    )
    
    env_lines = c(
      '# >>> conda initialize >>>',
      '# !! Contents within this block are managed by \'conda init\' !!',
      '__conda_setup="$(\'/Genomics/skinniderlab/conda/miniconda3/bin/conda\' \'shell.bash\' \'hook\' 2> /dev/null)"',
      'if [ $? -eq 0 ]; then',
      'eval "$__conda_setup"',
      'else',
      'if [ -f "/Genomics/skinniderlab/conda/miniconda3/etc/profile.d/conda.sh" ]; then',
      '. "/Genomics/skinniderlab/conda/miniconda3/etc/profile.d/conda.sh"',
      'else',
      'export PATH="/Genomics/skinniderlab/conda/miniconda3/bin:$PATH"',
      'fi',
      'fi',
      'unset __conda_setup',
      '# <<< conda initialize <<<',
      '',
      ifelse(grepl('^\\/', env),
             paste0('conda activate ', env),
             paste0('conda activate ', file.path(base_dir, env))
      ),
      '',
      'JOB_SIZE=$1',
      ''
    )
  }  else if (current_system == 'della') {
    log_dir = file.path(dirname(base_dir), 'logs', basename(base_dir))
    header_lines = c(
      '#!/bin/bash',
      paste0('#SBATCH --time=', time, ':00:00'),
      paste0('#SBATCH --job-name=', job_name),
      paste0('#SBATCH --output=', log_dir, '/%x-%j-%a.out'),
      paste0('#SBATCH --mem=', mem, 'G'),
      paste0('#SBATCH --cpus-per-task=', cpus),
      ifelse(gpu, paste0('#SBATCH --gres=gpu:1'), ''),
      ''
    )
    
    env_lines = c(
      '# >>> conda initialize >>>',
      '# !! Contents within this block are managed by \'conda init\' !!',
      '__conda_setup="$(\'/scratch/gfps/SKINNIDER/conda/miniconda3/bin/conda\' \'shell.bash\' \'hook\' 2> /dev/null)"',
      'if [ $? -eq 0 ]; then',
      'eval "$__conda_setup"',
      'else',
      'if [ -f "/scratch/gfps/SKINNIDER/conda/miniconda3/etc/profile.d/conda.sh" ]; then',
      '. "/scratch/gfps/SKINNIDER/conda/miniconda3/etc/profile.d/conda.sh"',
      'else',
      'export PATH="/scratch/gfps/SKINNIDER/conda/miniconda3/bin:$PATH"',
      'fi',
      'fi',
      'unset __conda_setup',
      '# <<< conda initialize <<<',
      '',
      ifelse(grepl('^\\/', env),
             paste0('conda activate ', env),
             paste0('conda activate ', file.path(base_dir, env))
      ),
      '',
      'JOB_SIZE=$1',
      ''
    )
  } else if (current_system == 'cedar') {
    log_dir = file.path(dirname(base_dir), 'logs', basename(base_dir))
    header_lines = c(
      '#!/bin/bash',
      paste0('#SBATCH --time=', time, ':00:00'),
      paste0('#SBATCH --job-name=', job_name),
      paste0('#SBATCH --output=', log_dir, '/%x-%j-%a.out'),
      paste0('#SBATCH --mem=', mem, 'G'),
      paste0('#SBATCH --cpus-per-task=', cpus),
      ifelse(gpu, paste0('#SBATCH --gres=gpu:1'), ''),
      ''
    )

    env_lines = c(
      '# >>> conda initialize >>>',
      '# !! Contents within this block are managed by \'conda init\' !!',
      '__conda_setup="$(\'/home/skinnim/projects/rrg-ljfoster-ab/skinnim/miniconda3/bin/conda\' \'shell.bash\' \'hook\' 2> /dev/null)"',
      'if [ $? -eq 0 ]; then',
      'eval "$__conda_setup"',
      'else',
      'if [ -f "/home/skinnim/projects/rrg-ljfoster-ab/skinnim/miniconda3/etc/profile.d/conda.sh" ]; then',
      '. "/home/skinnim/projects/rrg-ljfoster-ab/skinnim/miniconda3/etc/profile.d/conda.sh"',
      'else',
      'export PATH="/home/skinnim/projects/rrg-ljfoster-ab/skinnim/miniconda3/bin:$PATH"',
      'fi',
      'fi',
      'unset __conda_setup',
      '# <<< conda initialize <<<',
      '',
      ifelse(grepl('^\\/', env),
             paste0('conda activate ', env),
             paste0('conda activate ', file.path(base_dir, env))
      ),
      '',
      'JOB_SIZE=$1',
      ''
    )
  } else {
    stop('not sure how to write a sh file for: ', current_system)
  }

  # set up the final part of the script, which is platform-agnostic
  idx_var = switch(current_system,
                   'cedar' = 'SLURM_ARRAY_TASK_ID',
                   'lsi' = 'SLURM_ARRAY_TASK_ID',
                   'sockeye' = 'PBS_ARRAY_INDEX')
  run_lines = c(
    paste0('cd ', getwd()),
    '',
    paste0('START=$((($', idx_var, '-1)*$JOB_SIZE + 1))'),
    paste0('STOP=$((($', idx_var, '-1)*$JOB_SIZE+$JOB_SIZE))'),
    'for i in $( seq $START $STOP ); do',
    paste0('GRID_FILE=', grid_file),
    paste0('LINE_IDX=$((i + 1))'),
    'LINE=`sed "${LINE_IDX}q;d" $GRID_FILE`',
    'IFS=$\'\\t\' PARAMS=($LINE)',
    map_chr(seq_len(ncol(grid)), ~ {
      col_idx = .x
      colname = colnames(grid)[col_idx]
      param_line = paste0(colname, '=${PARAMS[', col_idx - 1, ']}')
      param_line
    }),
    '',
    switch(gsub("^.*\\.", "", trimws(inner_file)),
           'R' = paste0('Rscript ', inner_file, ' \\'),
           'py' = paste0('python ', inner_file, ' \\')
    ), 
    map_chr(seq_len(ncol(grid)), ~ {
      col_idx = .x
      colname = colnames(grid)[col_idx]
      if (col_idx < ncol(grid)) {
        arg_line = paste0('  --', colname, ' $', colname, ' \\')
      } else {
        arg_line = paste0('  --', colname, ' $', colname)
      }
      arg_line
    }),
    'done'
  )

  # write to file
  lines = c(header_lines,
            env_lines,
            run_lines)
  sh_file = switch(current_system,
                   cedar = sh_file,
                   lsi = sh_file,
                   sockeye = gsub("\\.sh", "", sh_file) %>%
                     paste0(., '.torque.sh'))
  sh_dir = dirname(sh_file)
  if (!dir.exists(sh_dir))
    dir.create(sh_dir, recursive = TRUE)
  writeLines(lines, sh_file)
}
