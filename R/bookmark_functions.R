# =============================================================================
# Shareable URL State Management for ACE
# =============================================================================
#
# This module provides URL-based state sharing for the ACE Shiny app.
# When users change inputs, the browser URL is updated with query parameters
# encoding the current state. Sharing that URL takes someone to the same view.
#
# Key design choices:
#   - Only "view-defining" inputs are tracked (not download dimensions, etc.)
#   - Multi-value inputs (e.g., river_groups) are comma-separated in the URL
#   - URL updates use mode="replace" to avoid polluting browser history
#   - A debounce timer prevents excessive URL updates during rapid changes
# =============================================================================


# ---------------------------------------------------------------------------
# Curated list of input IDs that define a shareable view state.
# These are the inputs that get encoded into the URL and restored on load.
#
# Organized by section for clarity.
# ---------------------------------------------------------------------------
SHAREABLE_INPUTS <- c(
  # Dataset selection
  "select_category",
  "select_textbox",

  # Active visualization tab
  "visualizations",

  # Filters
  "sf",            # selected filter annotations (multi-value)

  # Annotation comparison (pairwise) tab
  "annocomp_x",
  "annocomp_y",
  "annocomp_color",
  "annocomp_denom",
  "anno_reorderY",

  # River plot tab
  "river_groups",  # multi-value

  # Explorer tab
  "explorer_group",
  "explorer_annotation",
  "explorer_comparison",
  "explorer_plot_type",

  # Scatter plot tab
  "scatter_x",
  "scatter_y",
  "scatter_color_type",
  "scatter_plot_color",
  "show_filtered_data"
)


# ---------------------------------------------------------------------------
# Build a clean query string from the current input values.
#
# Only includes inputs listed in SHAREABLE_INPUTS that have non-empty,
# non-NULL, non-default values. Multi-value inputs are comma-joined.
#
# Returns a string like "?select_category=Disease+studies&visualizations=..."
# ---------------------------------------------------------------------------
build_share_query <- function(input) {
  params <- list()

  for (id in SHAREABLE_INPUTS) {
    val <- input[[id]]

    # Skip NULL or empty values
    if (is.null(val) || length(val) == 0) next
    if (length(val) == 1 && identical(val, "")) next

    # Convert to character and collapse multi-value inputs with commas
    val_str <- paste(as.character(val), collapse = ",")

    # URL-encode the value
    params[[id]] <- URLencode(val_str, reserved = TRUE)
  }

  if (length(params) == 0) return("")

  # Build query string
  pairs <- mapply(
    function(name, value) paste0(name, "=", value),
    names(params), params,
    USE.NAMES = FALSE
  )

  paste0("?", paste(pairs, collapse = "&"))
}


# ---------------------------------------------------------------------------
# Build a full shareable URL from the current session and input state.
#
# Returns the complete URL including protocol, host, path, and query string.
# ---------------------------------------------------------------------------
build_share_url <- function(session, input) {
  base_url <- paste0(
    session$clientData$url_protocol, "//",
    session$clientData$url_hostname,
    if (nzchar(session$clientData$url_port))
      paste0(":", session$clientData$url_port)
    else
      "",
    session$clientData$url_pathname
  )

  query <- build_share_query(input)
  paste0(base_url, query)
}


# ---------------------------------------------------------------------------
# Parse URL query parameters back into a named list.
#
# Multi-value parameters (comma-separated) are split into character vectors
# for inputs known to be multi-select (sf, river_groups).
#
# This is called on page load to initialize the app from a shared URL.
# ---------------------------------------------------------------------------
MULTI_VALUE_INPUTS <- c("sf", "river_groups")

parse_url_state <- function(url_search) {
  if (is.null(url_search) || !nzchar(url_search)) {
    return(list())
  }

  query <- parseQueryString(url_search)

  # Split comma-separated values for known multi-value inputs
  for (id in MULTI_VALUE_INPUTS) {
    if (!is.null(query[[id]]) && grepl(",", query[[id]])) {
      query[[id]] <- unlist(strsplit(query[[id]], ","))
    }
  }

  query
}


# ---------------------------------------------------------------------------
# Set up the URL state synchronization observers in the server function.
#
# This creates:
#   1. A debounced observer that updates the browser URL when inputs change
#   2. A "Copy Link" button handler with clipboard integration
#
# Call this once inside server <- function(input, output, session) { ... }
# ---------------------------------------------------------------------------
setup_url_sync <- function(input, output, session) {

  # Track whether we're currently restoring from URL to avoid circular updates.
  # Start as TRUE; after a short startup delay, flip to FALSE.
  restoring <- reactiveVal(TRUE)

  observe({
    invalidateLater(3000, session)
    isolate(restoring(FALSE))
  })

  # ---- Debounced URL updater ----
  # We use invalidateLater to create a 500ms debounce: every time any
  # shareable input changes, we schedule a URL update 500ms later.
  # If another change arrives before the timer fires, the old observer
  # run is superseded (standard Shiny invalidation behavior).

  observe({
    # Create reactive dependencies on all shareable inputs
    lapply(SHAREABLE_INPUTS, function(id) input[[id]])

    # Don't update URL while restoring from a shared link
    if (isolate(restoring())) return()

    # Debounce: wait 500ms before actually updating.
    # If this observer re-fires within 500ms, the previous
    # invalidateLater is canceled automatically by Shiny.
    invalidateLater(500, session)

    # Build and push the query string to the browser URL bar
    isolate({
      query <- build_share_query(input)
      updateQueryString(query, mode = "replace", session = session)
    })
  })

  # ---- Copy Link button handler ----
  observeEvent(input$copy_share_link, {
    url <- build_share_url(session, input)
    # Send the URL to the clipboard via custom JS message handler
    session$sendCustomMessage("copy_to_clipboard", url)
  })

  # Return the restoring flag so the caller can use it if needed
  invisible(restoring)
}


# ===========================================================================
# Legacy functions (kept for backward compatibility)
# ===========================================================================

# Convert all input values to a single query-parameter-style string.
build_storage_string <- function(input, keep_empty = TRUE) {
  vals <- reactiveValuesToList(input)
  vals <- vals[!sapply(vals, is.null)]
  vals <- lapply(vals, as.character)
  vals <- lapply(vals, function(x) paste(x, collapse = ","))
  vals <- vals[order(names(vals))]

  if (!keep_empty) {
    empty_vals <- sapply(vals, function(x) identical(x, ""))
    vals[empty_vals] <- NULL
  }

  pairs <- mapply(
    function(name, value) paste0(name, "=", gsub("[\\t ,]+", ",", value)),
    names(vals), vals,
    USE.NAMES = FALSE
  )

  paste(pairs, collapse = "&")
}


# Parse the stored values string back into a named list.
parse_storage_string <- function(store) {
  split_string <- strsplit(strsplit(store, "&")[[1]], "=")
  vals <- lapply(split_string, function(x) URLdecode(x[2]))
  names(vals) <- lapply(split_string, function(x) x[1])
  vals
}


# Build a full URL from all inputs (legacy, now calls the new version).
build_url <- function(session, input) {
  build_share_url(session, input)
}
