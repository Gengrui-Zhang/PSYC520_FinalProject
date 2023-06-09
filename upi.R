# Interaction-branch

#' Unconstrained product indicator method
#'
#' @param model A string variable describing the structural path model with interaction term(s),
#'              in \code{lavaan} syntax.
#' @param data A data frame containing indicator scores.
#' @return An object of classy7777777777777777777777777777777777777y \code{lavaan}.
#' @export

upi <- function (model, data) {
  #############################################################################
  # Helper function 1: Parsing the model and extract the interaction pairs
  # Model input example: y = x + m + x:m / Y = X + M + X:M
  # Model output should be a list of pairs of interaction terms

  interpairs <- function (model) {
    str_elements <- gsub(" ", "",
                         unlist(strsplit(unlist(strsplit(model, split = "\n|=~|~")),
                                         split = "+", fixed = TRUE)))
    inter_terms <- as.list(gsub("\n", "", str_elements[grep(":", str_elements)]))
    inter_vars <- list()
    for (i in seq(inter_terms)) {
      terms <- strsplit(inter_terms[[i]], split = ":")
      inter_vars[[i]] <- unlist(terms)
      names(inter_vars)[i] <- paste0("inter_pair_", i)
    }
    return(inter_vars)
  }

  pairs <- interpairs(model)

  #############################################################################
  # Helper function 2: Parsing the original model and extract the indicators of interaction terms
  # Model input: the original/full model
  # Model output should be a list of pairs of interaction terms

  indicators <- function (model) {
    model_elements <- unlist(strsplit(model, "\n"))
    indicator_terms <- unlist(strsplit(gsub(" ", "",
                                            model_elements[grep("=~",
                                                                unlist(strsplit(model, "\n")))]),
                                       split = "=~"))
    indicator_vars <- indicator_terms[grepl("+", indicator_terms, fixed = TRUE) == "FALSE"]
    indicator_items <- indicator_terms[grepl("+", indicator_terms, fixed = TRUE) == "TRUE"]
    indicators <- list()
    for (i in seq(indicator_items)) {
      indicators[[i]] <- unlist(strsplit(indicator_items[[i]], split = "+", fixed = TRUE))
      names(indicators)[i] <- paste0(indicator_vars[[i]])
    }
    return(indicators)
  }

  indics <- indicators(model)

  pairs_count <- length(pairs)

  # Generate product indicators for each interaction pairs
  #############################################################################
  # Helper function 3: Generate the indicator products and update the model
  product_syntax <- function (model, data, inter_pair, indicators) {
    inter_names <- inter_pair
    vec1 <- unname(unlist(indicators[inter_names][1]))
    vec2 <- unname(unlist(indicators[inter_names][2]))
    intNames <- paste0(rep(vec1, each = length(vec2)), vec2)

    # Update the model
    ind_1 <- vector()
    ind_syntax_vec <- list()
    ind_2 <- vector()
    ind_syntax_vec_2 <- list()
    count = 0

    for (n in vec1) {
      comb <- combn(c(intNames[grepl(n, intNames)]), 2)
      for (i in seq(unique(comb[1, ]))) {
        ind_1[i] <-  paste0("\n           ",
                            unique(comb[1, ])[i],
                            " ~~ ",
                            paste0("th", which(vec1 == n), "*",
                                   matrix(comb[,c(which(as.vector(comb[1, ])
                                                        == unique(comb[1, ])[i]))], nrow = 2)[2, ],
                                   collapse = " + ")
        )
      }
      ind_syntax_vec[[n]] <- ind_1
      count = count + 1
    }

    for (m in vec2) {
      comb <- combn(c(intNames[grepl(m, intNames)]), 2)
      for (i in seq(unique(comb[1, ]))) {
        ind_2[i] <-  paste0("\n           ",
                            unique(comb[1, ])[i],
                            " ~~ ",
                            paste0("th", which(vec2 == m) + count, "*",
                                   matrix(comb[,c(which(as.vector(comb[1, ])
                                                        == unique(comb[1, ])[i]))], nrow = 2)[2, ],
                                   collapse = " + "))
      }
      ind_syntax_vec_2[[m]] <- ind_2
    }

    model_indProd <- paste0("\n          #Indicator Products",
                            "\n          ",
                            paste(" ", paste(inter_names, collapse = "_int_"), "=~", paste(intNames, collapse = " + ")),
                            "\n          # Residual covariances in which same indicators have same constraint",
                            paste(unlist(ind_syntax_vec), collapse = " "),
                            "\n          ",
                            paste(unlist(ind_syntax_vec_2), collapse = " "),
                            "\n          ")
    return(model_indProd)
  }
  while (pairs_count > 0) {
    ind_syntax <- product_syntax(model, df, inter_pair = pairs[[pairs_count]], indicators = indics)
    model <- paste(gsub(":", "_int_", model),
                   ind_syntax)
    pairs_count <- pairs_count - 1
  }

  #############################################################################
  # Helper function 3: Generate the updated data with indicator products
  pairs_count <- length(pairs)
  # Generate a new data frame with indicators
  indprod_data <- function (data, inter_pair, indicators) {
    inter_names <- inter_pair
    vec1 <- unname(unlist(indicators[inter_names][1]))
    vec2 <- unname(unlist(indicators[inter_names][2]))
    intNames <- paste0(rep(vec1, each = length(vec2)), vec2)
    inter_data <- indProd(data = data, var1 = vec1,
                          var2 = vec2,
                          match = FALSE,
                          doubleMC = TRUE,
                          namesProd = intNames)
    inter_data <- inter_data[ , !names(inter_data) %in%
                        names(data)]
    return(inter_data)
  }
  while (pairs_count > 0) {
    interim_data <- indprod_data(data, pairs[[pairs_count]], indicators = indics)
    data <- cbind(data, interim_data)
    pairs_count <- pairs_count - 1
  }
  #############################################################################

  # Fit the model
  fit_indProd <- sem(model, data = data, std.lv = TRUE, meanstructure = TRUE)
  return(fit_indProd)
}
