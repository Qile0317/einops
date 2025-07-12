# # utility function to create a list of unknown axis lengths
make_unknown_composition <- function(x) {
    lapply(seq_along(x), function(i) list(known = integer(), unknown = i))
}
