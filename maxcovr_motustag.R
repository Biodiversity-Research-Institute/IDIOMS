

#modified maxcovr::mav_coverage function to not have to supply a starting position
extract_facility_selected_motustag <- function (solution_vector, A_mat, proposed_facilities) 
{
  I <- ncol(A_mat)
  facility_solution <- solution_vector[1:I]
  facility_id <- readr::parse_number(colnames(A_mat))
  facility_temp <- tibble::tibble(facility_id = facility_id, facility_chosen = facility_solution) %>% 
    dplyr::filter(facility_chosen == 1)

  facility_selected <- proposed_facilities %>% 
    dplyr::mutate(facility_id = facility_id) %>% 
    dplyr::filter(facility_id %in% facility_temp$facility_id) %>% 
    dplyr::select(-facility_id)
  return(facility_selected)
}

mc_mat_prep <- function (data) 
{
  as.matrix(data[, c("lat", "long")])
}

extract_mc_results_motustag <- function (x) 
{
  require(maxcovr)
  mc_facilities_selected <- extract_facility_selected_motustag(solution_vector = x$solution$solution, 
                                                              A_mat = x$A, proposed_facilities = x$proposed_facility)
  mc_users_affected <- extract_users_affected(A_mat = x$A, 
                                              solution_vector = x$solution$solution, user_id = x$user_id, 
                                              users_not_covered = x$user_not_covered)
  mc_augmented_users <- augment_user(facilities_selected = mc_facilities_selected, 
                                     existing_facilities = x$existing_facility, existing_users = x$existing_user)
  mc_model_coverage <- extract_model_coverage(augmented_user = mc_augmented_users, 
                                              distance_cutoff = x$distance_cutoff, n_added = x$n_added)
  # mc_existing_coverage <- extract_existing_coverage(existing_facilities = x$existing_facility, 
  #                                                   existing_users = x$existing_user, distance_cutoff = x$distance_cutoff)
  # mc_summary <- dplyr::bind_rows(mc_existing_coverage, mc_model_coverage)
  # mc_res <- tibble::tibble(n_added = list(x$n_added), distance_cutoff = list(x$distance_cutoff),
  #                          user_affected = list(mc_users_affected), augmented_users = list(mc_augmented_users),
  #                          facility_selected = list(mc_facilities_selected), model_coverage = list(mc_model_coverage),
  #                          existing_coverage = list(mc_existing_coverage), summary = list(mc_summary),
  #                          model_call = list(x$model_call), solution = list(x$solution))
  mc_summary <- mc_model_coverage
  mc_res <- tibble::tibble(n_added = list(x$n_added), distance_cutoff = list(x$distance_cutoff),
                           user_affected = list(mc_users_affected), augmented_users = list(mc_augmented_users),
                           facility_selected = list(mc_facilities_selected), model_coverage = list(mc_model_coverage)
                           , summary = list(mc_summary),
                           model_call = list(x$model_call), solution = list(x$solution))
  class(mc_res) <- c("maxcovr", class(mc_res))
  return(mc_res)
}

binary_distance_matrix_nanotag <- function (facility, user, distance_cutoff, d_proposed_user = NULL) 
{
  if (is.null(d_proposed_user)) {
    facility_cpp <- mc_mat_prep(facility)
    user_cpp <- mc_mat_prep(user)
    A <- binary_matrix_cpp(facility = facility_cpp, user = user_cpp, 
                           distance_cutoff = distance_cutoff)
  }
  else {
    d_proposed_user <- d_proposed_user[, user$user_id]
    d_proposed_user[is.na(d_proposed_user)] <- max(d_proposed_user, 
                                                   na.rm = TRUE)*10
    A <- t(d_proposed_user < distance_cutoff)
  }
  return(A)
}



max_coverage_motustag <- function(proposed_facility, user, distance_cutoff, 
                                  n_added, d_existing_user = NULL, d_proposed_user = NULL, 
                                  solver = "glpk") 
{
  require(maxcovr)
  user <- tibble::rowid_to_column(user, var = "user_id")
  # user_not_covered <- find_users_not_covered(existing_facility,
  #                                            user, distance_cutoff, d_existing_user = d_existing_user)
  #make the user not convered equal to all users since we want to start with no pre-selected
  user_not_covered <- user
  # A <- binary_distance_matrix(facility = proposed_facility, 
  #                             user = user_not_covered, distance_cutoff = distance_cutoff, 
  #                             d_proposed_user = d_proposed_user)
  # p <- profmem({
  A <- binary_distance_matrix_nanotag(facility = proposed_facility, 
                                      user = user, distance_cutoff = distance_cutoff, 
                                      d_proposed_user = d_proposed_user)
  
  colnames(A) <- 1:nrow(proposed_facility)
  # user_id_list <- 1:nrow(user_not_covered)
  user_id_list <- 1:nrow(user)
  Nx <- nrow(A)
  Ny <- ncol(A)
  c_vec <- c(rep(0, Ny), rep(1, Nx))
  d_vec <- c(rep(1, Ny), rep(0, Nx))
  Ain <- cbind(-A, diag(Nx))
  bin <- matrix(rep(0, Nx), ncol = 1)
  constraint_matrix <- rbind(Ain, d_vec)
  rhs_matrix <- rbind(bin, n_added)
  #free memory
  rm(list=c("Ain", "d_vec", "bin"))

  constraint_directions <- c(rep("<=", Nx), "==")
  if (solver == "lpSolve") {
    solution <- lpSolve::lp(direction = "max", objective.in = c_vec, 
                            const.mat = constraint_matrix, const.dir = constraint_directions, 
                            const.rhs = rhs_matrix, transpose.constraints = TRUE, 
                            all.bin = TRUE, num.bin.solns = 1, use.rw = TRUE)
    solution$solution <- as.integer(solution$solution)
  }
  
  if (solver == "glpk") {
    solution <- Rglpk::Rglpk_solve_LP(obj = c_vec, mat = constraint_matrix, 
                                      dir = constraint_directions, rhs = rhs_matrix, bounds = NULL, 
                                      types = "B", max = TRUE)
    solution$solution <- as.integer(solution$solution)
  }
  # if (solver == "gurobi") {
  #   if (!requireNamespace("gurobi", quietly = TRUE)) {
  #     stop("You must have installed the Gurobi software and accompanying\n                 Gurobi R package. For more details, see\n                 https://www.gurobi.com/documentation/7.0/refman/r_api_overview.html")
  #   }
  #   model <- list()
  #   model$A <- constraint_matrix
  #   model$obj <- c_vec
  #   model$sense <- c(rep("<=", Nx), "=")
  #   model$rhs <- rhs_matrix
  #   model$vtype <- "B"
  #   model$modelsense <- "max"
  #   solution <- gurobi::gurobi(model)
  # }
  model_call <- match.call()
  # x <- list(existing_facility = existing_facility, proposed_facility = proposed_facility, 
  #           distance_cutoff = distance_cutoff, existing_user = user, 
  #           user_not_covered = user_not_covered, n_added = n_added, 
  #           A = A, user_id = user_id_list, solution = solution, 
  #           model_call = model_call)
  x <- list(proposed_facility = proposed_facility, 
            distance_cutoff = distance_cutoff, existing_user = user,
            user_not_covered = user_not_covered,
            n_added = n_added, 
            A = A, user_id = user_id_list, solution = solution, 
            model_call = model_call)
  model_result <- extract_mc_results_motustag(x)  
  # rm(list("x", "constraint_matrix", "rhs_matrix", "c_vec"))
  # }, threshold = 100000)
  # print(p)
  return(model_result)
}


