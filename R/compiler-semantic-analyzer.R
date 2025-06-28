# #' @title Semantic Analyzer for Einops AST
# #' @description Analyze an EinopsAst and return a pure SemanticInfo S3 object
# #' @param ast EinopsAst object
# #' @param op_kind One of 'rearrange', 'reduce', 'repeat'
# #' @param axes_lengths_hint Named list of axis lengths (optional)
# #' @return SemanticInfo S3 object
# #' @keywords internal
# analyse_semantics <- function(
#     ast, op_kind = c("rearrange", "reduce", "repeat"), axes_lengths_hint = list()
# ) {

#     op_kind <- match.arg(op_kind)

#     # 1. Collect elementary axes and name2id
#     elem_result <- collect_elementaries(ast$input_axes)
#     elementaries <- elem_result$elementaries
#     name2id <- elem_result$name2id

#     # 2. Factorise inputs
#     input_factorisations <- factorise_inputs(ast$input_axes, name2id)

#     # 3. Scan output
#     scan_result <- scan_output(ast$output_axes, name2id, op_kind, elementaries)
#     elementaries <- scan_result$elementaries
#     added_axes <- scan_result$added_axes
#     output_seq <- scan_result$output_seq
#     output_compositions <- scan_result$output_compositions
#     reduced_axes <- scan_result$reduced_axes

#     # 4. Build permutation
#     axes_permutation <- build_permutation(elementaries, output_seq, reduced_axes)

#     # 5. Infer lengths
#     elementaries <- infer_lengths(elementaries, name2id, axes_lengths_hint)

#     # 6. Pack and return
#     pack_semantic_info(
#         elementaries = elementaries,
#         name2id = name2id,
#         input_factorisations = input_factorisations,
#         axes_permutation = axes_permutation,
#         reduced_axes = reduced_axes,
#         added_axes = added_axes,
#         output_compositions = output_compositions
#     )
# }

# UNKNOWN_LEN <- function() -1L

# ElementaryAxis <- function(id, name = NULL, length = UNKNOWN_LEN()) {
#     structure(list(id = id, name = name, length = length), class = "ElementaryAxis")
# }

# InputFactorisation <- function(dim_ix, factors) {
#     structure(list(dim_ix = dim_ix, factors = as.integer(factors)), class = "InputFactorisation")
# }

# OutputComposition <- function(dim_ix, factors) {
#     structure(list(dim_ix = dim_ix, factors = as.integer(factors)), class = "OutputComposition")
# }

# SemanticInfo <- function(
#     elementaries,
#     name2id,
#     input_factorisations,
#     axes_permutation,
#     reduced_axes,
#     added_axes,
#     output_compositions
# ) {
#     structure(list(
#         elementaries = elementaries,
#         name2id = name2id,
#         input_factorisations = input_factorisations,
#         axes_permutation = axes_permutation,
#         reduced_axes = reduced_axes,
#         added_axes = added_axes,
#         output_compositions = output_compositions
#     ), class = "SemanticInfo")
# }

# # 3.1 Collect elementary axes and name2id
# collect_elementaries <- function(input_axes) {
#     elementaries <- list()
#     name2id <- list()
#     id <- 0L
#     walk <- function(node) {
#         if (inherits(node, "NamedAxisAstNode")) {
#             elementaries[[length(elementaries) + 1L]] <<- ElementaryAxis(id, node$name, UNKNOWN_LEN())
#             if (!is.null(node$name)) name2id[[node$name]] <<- id
#             id <<- id + 1L
#         } else if (inherits(node, "ConstantAstNode")) {
#             elementaries[[length(elementaries) + 1L]] <<- ElementaryAxis(id, NULL, UNKNOWN_LEN())
#             id <<- id + 1L
#         } else if (inherits(node, "GroupAstNode")) {
#             for (child in node$children) walk(child)
#         } else if (inherits(node, "EllipsisAstNode")) {
#             elementaries[[length(elementaries) + 1L]] <<- ElementaryAxis(id, NULL, UNKNOWN_LEN())
#             id <<- id + 1L
#         } else {
#             stop("Unknown AST node type in collect_elementaries")
#         }
#     }
#     for (node in input_axes) walk(node)
#     list(elementaries = elementaries, name2id = name2id)
# }

# # Helper: flatten a node to axis ids (for factorisation)
# flatten_to_axis_ids <- function(node, name2id, id_acc = NULL) {
#     if (is.null(id_acc)) id_acc <- integer(0)
#     if (inherits(node, "NamedAxisAstNode")) {
#         id_acc <- c(id_acc, name2id[[node$name]])
#     } else if (inherits(node, "ConstantAstNode")) {
#         # Find the next available id (by order)
#         # This is only used in collect_elementaries, so here we just count order
#         # But for factorisation, we need to match the order in elementaries
#         # So, we assume the order is preserved
#         id_acc <- c(id_acc, NA_integer_)
#     } else if (inherits(node, "GroupAstNode")) {
#         for (child in node$children) id_acc <- flatten_to_axis_ids(child, name2id, id_acc)
#     } else if (inherits(node, "EllipsisAstNode")) {
#         id_acc <- c(id_acc, NA_integer_)
#     } else {
#         stop("Unknown AST node type in flatten_to_axis_ids")
#     }
#     id_acc
# }

# # 3.2 Factorise inputs
# factorise_inputs <- function(input_axes, name2id) {
#     input_factorisations <- list()
#     id_counter <- 0L
#     walk_ids <- function(node) {
#         ids <- integer(0)
#         if (inherits(node, "NamedAxisAstNode")) {
#             ids <- c(ids, name2id[[node$name]])
#         } else if (inherits(node, "ConstantAstNode")) {
#             ids <- c(ids, id_counter)
#             id_counter <<- id_counter + 1L
#         } else if (inherits(node, "GroupAstNode")) {
#             for (child in node$children) ids <- c(ids, walk_ids(child))
#         } else if (inherits(node, "EllipsisAstNode")) {
#             ids <- c(ids, id_counter)
#             id_counter <<- id_counter + 1L
#         } else {
#             stop("Unknown AST node type in factorise_inputs")
#         }
#         ids
#     }
#     for (dim_ix in seq_along(input_axes)) {
#         node <- input_axes[[dim_ix]]
#         factors <- walk_ids(node)
#         input_factorisations[[length(input_factorisations) + 1L]] <- InputFactorisation(dim_ix - 1L, factors)
#     }
#     input_factorisations
# }

# # 3.3 Scan output
# scan_output <- function(output_axes, name2id, op_kind, elementaries) {
#     output_seq <- integer(0)
#     added_axes <- integer(0)
#     output_compositions <- list()
#     reduced_axes <- integer(0)
#     all_names <- name2id
#     all_elementaries <- elementaries
#     next_id <- length(elementaries)
#     # Helper to get id for a node
#     get_id <- function(node) {
#         if (inherits(node, "NamedAxisAstNode")) {
#             nm <- node$name
#             if (!is.null(all_names[[nm]])) {
#                 return(all_names[[nm]])
#             } else if (op_kind == "repeat") {
#                 # New axis allowed only in repeat
#                 all_elementaries[[next_id + 1L]] <<- ElementaryAxis(next_id, NULL, UNKNOWN_LEN())
#                 added_axes <<- c(added_axes, next_id)
#                 names(added_axes)[length(added_axes)] <<- as.character(length(output_seq))
#                 all_names[[nm]] <<- next_id
#                 next_id <<- next_id + 1L
#                 return(all_names[[nm]])
#             } else {
#                 stop("Output axis '", nm, "' not found in input axes (only allowed in repeat)")
#             }
#         } else if (inherits(node, "ConstantAstNode")) {
#             all_elementaries[[next_id + 1L]] <<- ElementaryAxis(next_id, NULL, UNKNOWN_LEN())
#             added_axes <<- c(added_axes, next_id)
#             names(added_axes)[length(added_axes)] <<- as.character(length(output_seq))
#             next_id <<- next_id + 1L
#             return(next_id - 1L)
#         } else if (inherits(node, "GroupAstNode")) {
#             ids <- integer(0)
#             for (child in node$children) ids <- c(ids, get_id(child))
#             return(ids)
#         } else if (inherits(node, "EllipsisAstNode")) {
#             all_elementaries[[next_id + 1L]] <<- ElementaryAxis(next_id, NULL, UNKNOWN_LEN())
#             added_axes <<- c(added_axes, next_id)
#             names(added_axes)[length(added_axes)] <<- as.character(length(output_seq))
#             next_id <<- next_id + 1L
#             return(next_id - 1L)
#         } else {
#             stop("Unknown AST node type in scan_output")
#         }
#     }
#     # Walk output axes
#     for (node in output_axes) {
#         ids <- get_id(node)
#         output_seq <- c(output_seq, ids)
#     }
#     # Reduced axes (for reduce)
#     all_input_ids <- unlist(lapply(elementaries, function(ax) ax$id))
#     if (op_kind == "reduce") {
#         reduced_axes <- setdiff(all_input_ids, output_seq)
#     }
#     # Output compositions: group contiguous identical top-level nodes
#     for (dim_ix in seq_along(output_axes)) {
#         node <- output_axes[[dim_ix]]
#         ids <- get_id(node)
#         output_compositions[[length(output_compositions) + 1L]] <- OutputComposition(dim_ix - 1L, ids)
#     }
#     list(
#         elementaries = all_elementaries,
#         added_axes = added_axes,
#         output_seq = output_seq,
#         output_compositions = output_compositions,
#         reduced_axes = reduced_axes
#     )
# }

# # 3.4 Build permutation
# build_permutation <- function(elementaries, output_seq, reduced_axes) {
#     initial_order <- vapply(elementaries, function(ax) ax$id, integer(1))
#     full_output <- c(output_seq, reduced_axes)
#     match(full_output, initial_order)
# }

# # 3.5 Infer lengths
# infer_lengths <- function(elementaries, name2id, axes_lengths_hint) {
#     for (nm in names(axes_lengths_hint)) {
#         len <- axes_lengths_hint[[nm]]
#         id <- name2id[[nm]]
#         if (!is.null(id)) {
#             old <- elementaries[[id + 1L]]$length
#             if (old != UNKNOWN_LEN() && old != len) {
#                 stop("Conflicting length hint for axis ", nm)
#             }
#             elementaries[[id + 1L]]$length <- len
#         }
#     }
#     elementaries
# }

# # 3.6 Pack semantic info
# pack_semantic_info <- function(...) {
#     SemanticInfo(...)
# }

# #' @export
# print.SemanticInfo <- function(x, ...) {
#     cat("SemanticInfo object\n")
#     # Reconstruct input axes
#     elementaries <- x$elementaries
#     input_factorisations <- x$input_factorisations
#     axis_strs <- lapply(input_factorisations, function(fac) {
#         ids <- fac$factors
#         axes <- vapply(ids, function(id) {
#             ax <- elementaries[[id + 1L]]
#             if (!is.null(ax$name)) ax$name else "."
#         }, character(1))
#         paste(axes, collapse = " ")
#     })
#     input_str <- paste(axis_strs, collapse = ", ")
#     cat("Input axes: ", input_str, "\n", sep = "")
#     cat("Other fields:\n")
#     cat("  axes_permutation:", paste(x$axes_permutation, collapse = ", "), "\n")
#     cat("  reduced_axes:", paste(x$reduced_axes, collapse = ", "), "\n")
#     cat("  added_axes:", paste(x$added_axes, collapse = ", "), "\n")
#     cat("  output_compositions: [list of length ", length(x$output_compositions), "]\n", sep = "")
#     invisible(x)
# }
