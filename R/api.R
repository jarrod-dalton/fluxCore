# ------------------------------------------------------------------------------
# new_entity()
#
# Purpose:
#   Convenience constructor for an Entity R6 object. Designed to make it easy to
#   create an entity from row-like inputs (e.g., a 1-row data.frame).
#
# Parameters:
#   init        Initial state overrides. Supported:
#                - named list (preferred)
#                - named atomic vector
#                - 1-row data.frame
#                - NULL (treated as empty list)
#   schema      Entity schema (see default_entity_schema()).
#   time0       Initial event time (numeric).
#   event_type0 Event type label for the initial event.
#
# Returns:
#   An Entity R6 object.
# ------------------------------------------------------------------------------

new_entity <- function(init = list(),
                        schema = default_entity_schema(),
                        entity_type = NULL,
                        time0 = 0,
                        event_type0 = "init") {

  # Accept common row-like inputs:
  # - named list (preferred)
  # - named atomic vector
  # - 1-row data.frame
  if (is.data.frame(init)) {
    if (nrow(init) != 1L) stop("If init is a data.frame it must have exactly one row.", call. = FALSE)
    init <- as.list(init[1, , drop = FALSE])
  } else if (!is.list(init) && !is.null(init)) {
    if (is.atomic(init) && !is.null(names(init))) {
      init <- as.list(init)
    } else {
      stop("init must be a named list, a named atomic vector, a 1-row data.frame, or NULL.", call. = FALSE)
    }
  }

  if (is.null(init)) init <- list()

  Entity$new(
    init = init,
    schema = schema,
    entity_type = entity_type,
    time0 = time0,
    event_type0 = event_type0
  )
}
