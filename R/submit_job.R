#' Submit a job to the scheduler
#'
#' Submit an array of jobs to the scheduler on a high-performance computing 
#' cluster. 
#' 
#' @param grid path to the tab-delimited grid file
#' @param script path to write the submission script to
#' @param allocation optional, account name to use on the platform in question
#' @param job_loop how many rows in the grid file should be run in one array
#'   job?
#'
#' @importFrom magrittr `%<>%`
#'
#' @export
submit_job = function(grid, script, allocation = NULL, job_loop = 1,
                      jobs_per_array = 100) {
  args = as.list(match.call())
  if (!is.null(args$allocation))
    alloc <<- eval(args$allocation)
  
  ## detect system if not already done
  detect_system()
  if (!is.null(alloc))
    allocation <<- alloc
  
  if (current_system == 'cedar') {
    n_jobs = ceiling(nrow(grid)/ job_loop)
    system(paste0("cd ~/project; ",
                  "sbatch --account=", allocation, " --array=1-", n_jobs,
                  " ", script, " ", job_loop))
  } else if (current_system %in% c('lsi', 'della')) {
    n_jobs = ceiling(nrow(grid)/ job_loop)
    system(paste0("sbatch --array=1-", n_jobs, " ", script, " ", job_loop))
  } else if (current_system == 'sockeye') {
    wd = getwd()
    if (file.exists(file.path(wd, script)))
      script %<>% file.path(wd, .)
    n_jobs = ceiling(nrow(grid)/ job_loop)
    system(paste0("cd /scratch; ",
                  "sbatch --account=", allocation, " --array=1-", n_jobs,
                  " ", script, " ", job_loop))
  } else if (current_system == 'sockeye') {
    script %<>% gsub("\\.sh$", ".torque.sh", .)
    n_jobs = ceiling(nrow(grid)/ job_loop)
    # avoid a torque bug
    if (n_jobs == 1) {
      n_jobs = 2
    }
    jobs_per_array = 100
    ## Sockeye only lets you run 1,000 jobs at a time
    n_submissions = ifelse(n_jobs > jobs_per_array,
                           ceiling(n_jobs / jobs_per_array), 1)
    for (submission_idx in seq_len(n_submissions)) {
      job_start = (submission_idx - 1) * jobs_per_array + 1
      job_end = ifelse(submission_idx == n_submissions,
                       ifelse(n_jobs %% jobs_per_array == 0,
                              submission_idx * jobs_per_array,
                              job_start - 1 + n_jobs %% jobs_per_array),
                       submission_idx * jobs_per_array)
      system(paste0("qsub -A ", allocation, " -J ", job_start, "-", job_end,
                    " -v ", shQuote(paste0("JOB_SIZE=", job_loop)), " ", script))
    }
  } else if (current_system == 'elasti') {
    n_jobs = ceiling(nrow(grid)/ job_loop)
    script %<>% gsub("\\.sh$", ".elasti.sh", .)
    system(paste0("cd ~/project; ",
                  "sbatch --account=", allocation, " --array=1-", n_jobs,
                  " ", script, " ", job_loop))
  }
}
