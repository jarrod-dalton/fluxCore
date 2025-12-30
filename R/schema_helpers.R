schema_var <- function(default,
                       blocks = "state",
                       coerce = function(x) x,
                       validate = function(x) TRUE,
                       desc = NULL) {
  # Small helper for defining state variables in a patient schema.
  #
  # A schema is a named list of variable definitions. Each variable definition
  # is itself a list that can specify:
  #   - default: value used when the init data do not provide the variable
  #   - blocks: which update blocks this variable belongs to ("state" by default)
  #   - coerce: function to coerce incoming values
  #   - validate: function(x) -> TRUE/FALSE
  #   - desc: optional short description (used in educational model packages)
  list(
    default = default,
    blocks = blocks,
    coerce = coerce,
    validate = validate,
    desc = desc
  )
}
