#' Detect the system we are working on
#'
#' Detect the current system (e.g., sockeye, LSI cluster, cedar) and set the
#' base directory accordingly. 
#'
#' @param project_name optionally, specify the name of the current project.
#'   If unset, will be inferred from the working directory 
#'
#' @return assigns values to global variables \code{current_system} and 
#'   \code{base_dir}
#'
#' @export
detect_system = function(project_name = NULL) {
  ## detect system
  current_system <<- 'sockeye' ## default
  node = Sys.info()[["nodename"]]
  if (grepl("cedar", node)) {
    current_system <<- 'cedar'
  } else if (grepl("frontend|worker", node)) {
    current_system <<- 'elasti'
  } else if (grepl('argo', node)) {
    current_system <<- 'lsi'
  } else if (grepl('della', node)) {
    current_system <<- 'della'
  } else if (Sys.info()["sysname"] == 'Darwin') {
    current_system <<- 'local'
  }
  
  # set up base directory
  wd = getwd()
  if (is.null(project_name))
    project_name = basename(wd) ## ~/git/_project_
  if (current_system == 'cedar') {
    base_dir <<- "/home/aphil/projects/rrg-ljfoster-ab/skinnim"
    allocation <<- 'rrg-aphil'
  } else if (current_system == 'sockeye') {
    base_dir <<- "/scratch/st-ljfoster-1"
    allocation <<- 'st-ljfoster-1'
  } else if (current_system == 'elasti') {
    base_dir <<- '/home/ubuntu/projects/rrg-ljfoster-ab/skinnim'
  } else if (current_system == 'lsi') {
    base_dir <<- '/Genomics/skinniderlab'
    allocation <<- 'root'
  } else if (current_system == 'della') {
    base_dir <<- '/scratch/gpfs/SKINNIDER'
    allocation <<- 'root'
  } else if (current_system == 'local') {
    base_dir <<- file.path("~/git", project_name, "data")
  }
  if (current_system != 'local')
    base_dir <<- file.path(base_dir, project_name)
}
