#' Detect the system we are working on
#'
#' Detect the current system (e.g., sockeye, LSI cluster, cedar) and set the
#' base directory accordingly. 
#'
#' @param project_name optionally, specify the name of the current project.
#'   If unset, will be inferred from the working directory 
#'
#' @return assigns values to global variables \code{system} and \code{base_dir}
#'
#' @export
detect_system = function(project_name = NULL) {
  ## detect system
  system <<- 'sockeye' ## default
  node = Sys.info()[["nodename"]]
  if (grepl("cedar", node)) {
    system <<- 'cedar'
  } else if (grepl("frontend|worker", node)) {
    system <<- 'elasti'
  } else if (grepl('argo', node)) {
    system <<- 'lsi'
  } else if (Sys.info()["sysname"] == 'Darwin') {
    system <<- 'local'
  }
  
  # set up base directory
  wd = getwd()
  if (is.null(project_name))
    project_name = basename(wd) ## ~/git/_project_
  if (system == 'cedar') {
    base_dir <<- "/home/aphil/projects/rrg-ljfoster-ab/skinnim"
    allocation <<- 'rrg-aphil'
  } else if (system == 'sockeye') {
    base_dir <<- "/scratch/st-ljfoster-1"
    allocation <<- 'st-ljfoster-1'
  } else if (system == 'elasti') {
    base_dir <<- '/home/ubuntu/projects/rrg-ljfoster-ab/skinnim'
  } else if (system == 'lsi') {
    base_dir <<- '/Genomics/skinniderlab'
    allocation <<- 'root'
  } else if (system == 'local') {
    base_dir <<- file.path("~/git", project_name, "data")
  }
  base_dir <<- file.path(base_dir, project_name)
}
