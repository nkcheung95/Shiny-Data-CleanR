# SHINY DATA CLEANR v0.2
packages <- c("tidyverse", "zoo", "scales", "ggplot2", "shiny", "signal", "readxl", "ragg")

install_load_packages <- function(packages) {
  not_installed <- setdiff(packages, rownames(installed.packages()))
  if (length(not_installed) > 0) install.packages(not_installed, repos = "https://cloud.r-project.org")
  invisible(sapply(packages, library, character.only = TRUE))
}
install_load_packages(packages)


# ── Helpers ────────────────────────────────────────────────────────────────────

pchip_interp <- function(x) {
  idx   <- seq_along(x)
  valid <- !is.na(x)
  if (sum(valid) < 2) return(x)
  f          <- splinefun(idx[valid], x[valid], method = "monoH.FC")
  x[!valid]  <- f(idx[!valid])
  x
}

reflect_pad <- function(x, n) {
  n <- min(n, length(x) - 1)
  c(rev(x[2:(n + 1)]), x, rev(x[(length(x) - n):(length(x) - 1)]))
}

unpad <- function(x, n) x[(n + 1):(length(x) - n)]

smart_breaks <- function(x_max) {
  if (!is.finite(x_max) || x_max <= 0) return(0)
  interval <- dplyr::case_when(
    x_max <= 120  ~ 10,
    x_max <= 600  ~ 30,
    x_max <= 1800 ~ 60,
    x_max <= 7200 ~ 300,
    TRUE          ~ 600
  )
  seq(0, ceiling(x_max / interval) * interval, by = interval)
}

# Convert any time format to seconds
# Handles: numeric seconds, numeric minutes, "MM:SS", "HH:MM:SS"
parse_time_to_seconds <- function(x) {
  x_char <- as.character(x)

  # Check if values contain colons → timestamp format
  has_colon <- grepl(":", x_char, fixed = TRUE)

  if (any(has_colon, na.rm = TRUE)) {
    parts <- strsplit(x_char, ":")
    secs <- sapply(parts, function(p) {
      p <- suppressWarnings(as.numeric(p))
      if (any(is.na(p))) return(NA_real_)
      if (length(p) == 2) p[1] * 60 + p[2]          # MM:SS
      else if (length(p) == 3) p[1] * 3600 + p[2] * 60 + p[3]  # HH:MM:SS
      else NA_real_
    })
    return(as.numeric(secs))
  }

  # Numeric — decide seconds vs minutes by magnitude
  num <- suppressWarnings(as.numeric(x_char))
  # If max value < 200, likely minutes (FMD rarely > 3 hours in seconds)
  # If max value > 200, likely already seconds
  max_val <- max(num, na.rm = TRUE)
  if (is.finite(max_val) && max_val < 200) {
    num * 60   # convert minutes → seconds
  } else {
    num        # already seconds
  }
}

# Auto-detect header row: scan until we find a row where >50% of cells
# are non-empty and non-numeric (i.e. column names)
find_header_row <- function(raw_lines, max_scan = 20) {
  n <- min(max_scan, length(raw_lines))
  for (i in seq_len(n)) {
    cells <- strsplit(raw_lines[i], "[,;\t]")[[1]]
    cells <- trimws(cells)
    non_empty <- cells[nzchar(cells)]
    if (length(non_empty) < 2) next
    n_numeric <- sum(!is.na(suppressWarnings(as.numeric(non_empty))))
    frac_text <- 1 - n_numeric / length(non_empty)
    if (frac_text >= 0.5) return(i)
  }
  return(1L)  # fallback
}

# Sniff delimiter from first data-containing line
sniff_delimiter <- function(raw_lines, header_row) {
  line <- raw_lines[header_row]
  counts <- c(
    comma     = nchar(line) - nchar(gsub(",",  "", line, fixed = TRUE)),
    semicolon = nchar(line) - nchar(gsub(";",  "", line, fixed = TRUE)),
    tab       = nchar(line) - nchar(gsub("\t", "", line, fixed = TRUE))
  )
  c(comma = ",", semicolon = ";", tab = "\t")[names(which.max(counts))]
}


# ── Smart file loader ──────────────────────────────────────────────────────────

smart_load <- function(path, ext, user_header_row = 0) {

  if (ext %in% c("xlsx", "xls")) {
    # For Excel: read all rows without header, find header row
    raw <- suppressMessages(read_excel(path, col_names = FALSE, .name_repair = "minimal"))
    raw_lines <- apply(raw, 1, paste, collapse = ",")
    
    if (is.null(user_header_row) || user_header_row == 0) {
      hrow  <- find_header_row(raw_lines)
    } else {
      hrow  <- user_header_row
    }
    
    df    <- suppressMessages(read_excel(path, skip = hrow - 1, .name_repair = "unique"))
    
    # Safe patch: Prevent empty names from crashing dplyr::rename later
    bad_names <- colnames(df) == "" | is.na(colnames(df))
    if (any(bad_names)) colnames(df)[bad_names] <- paste0("Unnamed_", which(bad_names))
    
    return(df)
  }

  # Text-based: read raw lines first
  raw_lines <- readLines(path, warn = FALSE)
  raw_lines_no_blank <- raw_lines[nzchar(trimws(raw_lines))]  # drop blank lines for scanning

  if (is.null(user_header_row) || user_header_row == 0) {
    hrow  <- find_header_row(raw_lines_no_blank)
    delim <- sniff_delimiter(raw_lines_no_blank, hrow)

    # Re-read the original file (with blanks) using detected skip + delimiter
    # Count how many lines to skip in the ORIGINAL file
    all_lines  <- readLines(path, warn = FALSE)
    # Find which original line index matches our hrow in the non-blank version
    non_blank_idx <- which(nzchar(trimws(all_lines)))
    orig_hrow     <- non_blank_idx[hrow]
    skip_n        <- orig_hrow - 1
  } else {
    hrow  <- user_header_row
    if (hrow <= length(raw_lines)) {
      delim <- sniff_delimiter(raw_lines, hrow)
    } else {
      delim <- ","
    }
    skip_n <- hrow - 1
  }

  df <- read.delim(path, sep = delim, header = TRUE, skip = skip_n,
                   stringsAsFactors = FALSE, check.names = FALSE)

  # Drop columns that are entirely NA (common with wide CSV exports)
  df <- df[, colSums(!is.na(df)) > 0, drop = FALSE]

  # Safe patch: Prevent empty names from crashing dplyr::rename later
  bad_names <- colnames(df) == "" | is.na(colnames(df))
  if (any(bad_names)) colnames(df)[bad_names] <- paste0("Unnamed_", which(bad_names))

  df
}


# ── Cleaning methods ───────────────────────────────────────────────────────────

clean_median_iqr <- function(df, window_size, threshold) {
  x      <- df$data
  pad_n  <- window_size
  padded <- reflect_pad(x, pad_n)

  med_pad       <- rollmedian(padded, window_size, align = "center", fill = NA)
  moving_median <- unpad(med_pad, pad_n)

  iqr_val <- IQR(moving_median, na.rm = TRUE)
  outlier  <- abs(x - moving_median) > (threshold * iqr_val)

  interp <- x
  interp[outlier | is.na(x)] <- NA
  interp <- pchip_interp(interp)

  df$moving_median <- moving_median
  df$outlier       <- outlier
  df$clean_data    <- interp
  df[!is.na(df$clean_data), ]
}

clean_butterworth <- function(df, sample_rate, cutoff_hz, order) {
  x        <- pchip_interp(df$data)
  nyq      <- sample_rate / 2
  norm_cut <- min(max(cutoff_hz / nyq, 1e-4), 0.9999)
  bf       <- butter(order, norm_cut, type = "low")

  filt_len <- max(length(bf$b), length(bf$a))
  pad_n    <- max(3 * filt_len, round(sample_rate * 2))
  padded   <- reflect_pad(x, pad_n)
  filtered <- unpad(filtfilt(bf, padded), pad_n)

  df$outlier    <- FALSE
  df$clean_data <- filtered
  df[!is.na(df$clean_data), ]
}

clean_combined <- function(df, window_size, threshold, sample_rate, cutoff_hz, order) {
  step1         <- clean_median_iqr(df, window_size, threshold)
  outlier_flags <- step1$outlier
  step1$data    <- step1$clean_data
  step2         <- clean_butterworth(step1, sample_rate, cutoff_hz, order)
  step2$outlier <- outlier_flags[seq_len(nrow(step2))]
  step2
}


# ── UI ─────────────────────────────────────────────────────────────────────────

ui <- fluidPage(
  titlePanel("Shiny Data CleanR v0.2 - nkcheung95"),

  sidebarLayout(
    sidebarPanel(
      width = 3,

      h4("File"),
      fileInput("file1", "CSV / TXT / XLSX",
                accept = c(".csv", ".txt", ".xlsx", ".xls")),

      # Dynamically generated header selection dropdown
      uiOutput("header_row_selector"),

      # Show detected format info
      uiOutput("file_detect_info"),

      hr(),
      h4("Columns"),
      uiOutput("column_selectors"),

      hr(),
      h4("Display"),
      textInput("vlines_input", "Vertical line positions (seconds, max 5)", value = "60, 360"),
      helpText("Separate values with commas. Leave blank for no lines."),

      hr(),
      downloadButton("download_data", "Download Cleaned Data")
    ),

    mainPanel(
      width = 9,

      wellPanel(
        h4("Smoothing Method"),
        radioButtons("method", NULL,
          choices = c(
            "Median + IQR"                    = "median_iqr",
            "Butterworth Low-Pass"            = "butterworth",
            "Median + IQR  →  Butterworth"    = "combined"
          ),
          inline = TRUE
        ),

        fluidRow(
          conditionalPanel(
            condition = "input.method == 'median_iqr' || input.method == 'combined'",
            column(4,
              h5("Median + IQR"),
              sliderInput("window_size", "Window size (odd samples)",
                          min = 3, max = 501, value = 11, step = 2, ticks = FALSE),
              sliderInput("iqr_threshold", "IQR threshold multiplier",
                          min = 0, max = 5, value = 1.5, step = 0.25, ticks = FALSE)
            )
          ),
          conditionalPanel(
            condition = "input.method == 'butterworth' || input.method == 'combined'",
            column(4,
              h5("Butterworth Low-Pass"),
              uiOutput("cutoff_slider"), 
              sliderInput("butter_order", "Filter order",
                          min = 1, max = 8, value = 4, step = 1, ticks = FALSE)
            )
          ),
          column(4,
            h5("Interpolation"),
            helpText(strong("PCHIP"), "— monotone piecewise cubic Hermite.",
                     "Preserves shape and avoids oscillation near gaps.")
          )
        )
      ),

      verbatimTextOutput("file_info"),
      plotOutput("raw_plot",   height = "340px"),
      plotOutput("clean_plot", height = "340px")
    )
  )
)


# ── Server ─────────────────────────────────────────────────────────────────────

server <- function(input, output, session) {

  # ── Parse Custom Vertical Lines ──
  parsed_vlines <- reactive({
    if (is.null(input$vlines_input) || trimws(input$vlines_input) == "") return(numeric(0))
    
    # Split text by commas, turn into numbers, drop any failed parses (NAs)
    vals <- unlist(strsplit(input$vlines_input, ","))
    vals <- suppressWarnings(as.numeric(trimws(vals)))
    vals <- vals[!is.na(vals)]
    
    # Enforce maximum limit of 5 entries
    if (length(vals) > 5) {
      vals <- vals[1:5]
    }
    vals
  })

  # ── Raw file scanning for header preview dropdown ──
  file_preview <- reactive({
    req(input$file1)
    ext  <- tolower(tools::file_ext(input$file1$name))
    path <- input$file1$datapath
    
    if (ext %in% c("xlsx", "xls")) {
      raw <- suppressMessages(read_excel(path, col_names = FALSE, n_max = 20, .name_repair = "minimal"))
      raw_lines <- apply(raw, 1, function(row) {
        paste(na.omit(as.character(row)), collapse = ", ")
      })
      return(as.character(raw_lines))
    } else {
      return(readLines(path, n = 20, warn = FALSE))
    }
  })

  # Generate dropdown listing row options with contents previewed
  output$header_row_selector <- renderUI({
    req(file_preview())
    raw_lines <- file_preview()
    
    clean_lines <- gsub("[\t;]", ", ", raw_lines)
    clean_lines <- trimws(clean_lines)
    
    labels <- paste0("Row ", seq_along(clean_lines), ": ", 
                     ifelse(clean_lines == "", "[Empty Row]", 
                            ifelse(nchar(clean_lines) > 55, 
                                   paste0(substr(clean_lines, 1, 52), "..."), 
                                   clean_lines)))
    
    choices        <- seq_along(clean_lines)
    names(choices) <- labels
    choices        <- c("Auto-detect Header" = 0, choices)
    
    selectInput("header_row", "Header row setting", choices = choices, selected = 0)
  })

  # ── Load & detect ──
  loaded <- reactive({
    req(input$file1)
    ext <- tolower(tools::file_ext(input$file1$name))
    
    h_row <- if (is.null(input$header_row)) 0 else as.numeric(input$header_row)
    
    tryCatch(
      smart_load(input$file1$datapath, ext, user_header_row = h_row),
      error = function(e) {
        showNotification(paste("Load error:", e$message), type = "error", duration = 10)
        NULL
      }
    )
  })

  output$file_detect_info <- renderUI({
    req(loaded())
    df <- loaded()
    helpText(sprintf("Detected: %d rows × %d columns", nrow(df), ncol(df)))
  })

  # ── Column selectors — auto-guess time & data cols ──
  output$column_selectors <- renderUI({
    req(loaded())
    cols <- colnames(loaded())

    time_guess <- cols[grep("^time|^t$|\\[min|\\[sec|\\[s\\]", cols,
                            ignore.case = TRUE, perl = TRUE)[1]]
    if (is.na(time_guess)) time_guess <- cols[1]

    data_guess <- cols[grep("diameter|diam", cols, ignore.case = TRUE)[1]]
    if (is.na(data_guess)) data_guess <- cols[min(2, length(cols))]

    tagList(
      selectInput("time_col", "Time column",
                  choices = cols, selected = time_guess),
      selectInput("data_col", "Data column",
                  choices = cols, selected = data_guess),
      selectInput("comments_col", "Comments column (optional)",
                  choices = c("None", cols), selected = "None")
    )
  })

  # ── Build working dataframe ──
  working <- reactive({
    req(loaded(), input$time_col, input$data_col)
    df <- loaded()
    df <- rename(df, time = !!input$time_col, data = !!input$data_col)
    if (!is.null(input$comments_col) && input$comments_col != "None")
      df <- rename(df, comments = !!input$comments_col)

    df$time <- parse_time_to_seconds(df$time)
    df$data <- suppressWarnings(as.numeric(as.character(df$data)))
    df <- df[!is.na(df$time) & !is.na(df$data), ]
    df$time <- df$time - min(df$time)
    df
  })

  sample_rate <- reactive({
    req(working())
    dt <- median(diff(working()$time), na.rm = TRUE)
    if (is.na(dt) || dt <= 0) return(1)
    1 / dt
  })

  output$cutoff_slider <- renderUI({
    nyq     <- sample_rate() / 2
    max_cut <- min(30, round(nyq * 0.95, 3))
    def_cut <- min(1.0, max_cut)
    
    sliderInput("butter_cutoff", "Cutoff frequency (Hz)",
                min = 0.001, max = max(max_cut, 0.01), value = def_cut,
                step = 0.001, ticks = FALSE)
  })

  output$file_info <- renderPrint({
    req(working(), nrow(working()) > 0)
    df <- working()
    raw_time <- as.character(loaded()[[input$time_col]])[1:3]
    fmt <- if (any(grepl(":", raw_time))) "MM:SS / HH:MM:SS (auto-converted)"
           else if (max(working()$time) < 200 * 60) "decimal minutes (auto-converted)"
           else "seconds"
    cat(sprintf(
      "Rows: %d  |  Time: %.0f – %.0f s  |  Sample rate ≈ %.2f Hz  |  Time format: %s\n",
      nrow(df), min(df$time), max(df$time), sample_rate(), fmt
    ))
  })

  fmd_theme <- function() {
    theme_minimal(base_size = 13) +
      theme(legend.position = c(0.88, 0.15),
            legend.background = element_rect(fill = alpha("white", 0.7), color = NA))
  }

  output$raw_plot <- renderPlot({
    req(working(), nrow(working()) > 0)
    df   <- working()
    lines <- parsed_vlines()
    
    p <- ggplot(df, aes(x = time, y = data)) +
      geom_point(size = 0.7, color = "steelblue", alpha = 0.6) +
      scale_x_continuous(breaks = smart_breaks(max(df$time, na.rm = TRUE)), labels = comma) +
      labs(x = "Time (seconds)", y = input$data_col, title = "Raw Data") +
      fmd_theme()
      
    if (length(lines) > 0) {
      p <- p + geom_vline(xintercept = lines, color = "red", linetype = "dashed")
    }
    p
  })

  cleaned_data <- reactive({
    req(working(), input$method)
    sr <- sample_rate()
    
    cutoff_hz <- 1.0
    if (input$method %in% c("butterworth", "combined")) {
      req(input$butter_cutoff)
      cutoff_hz <- input$butter_cutoff
    }
    
    tryCatch(
      switch(input$method,
        median_iqr  = clean_median_iqr(working(), input$window_size, input$iqr_threshold),
        butterworth = clean_butterworth(working(), sr, cutoff_hz, input$butter_order),
        combined    = clean_combined(working(), input$window_size, input$iqr_threshold,
                                     sr, cutoff_hz, input$butter_order)
      ),
      error = function(e) {
        showNotification(paste("Cleaning error:", e$message), type = "error", duration = 8)
        NULL
      }
    )
  })

  output$clean_plot <- renderPlot({
    req(cleaned_data(), nrow(cleaned_data()) > 0)
    df    <- cleaned_data()
    meth  <- input$method
    lines <- parsed_vlines()

    subtitle <- switch(meth,
      median_iqr  = paste0("Median+IQR  |  window=", input$window_size,
                           " samples,  threshold=", input$iqr_threshold, "×IQR"),
      butterworth = paste0("Butterworth  |  cutoff=", input$butter_cutoff,
                           " Hz,  order=", input$butter_order),
      combined    = paste0("Median+IQR (w=", input$window_size, ", t=", input$iqr_threshold,
                           ")  →  Butterworth (cutoff=", input$butter_cutoff,
                           " Hz, order ", input$butter_order, ")")
    )

    p <- ggplot(df, aes(x = time, y = clean_data)) +
      scale_x_continuous(breaks = smart_breaks(max(df$time, na.rm = TRUE)), labels = comma) +
      labs(x = "Time (seconds)", y = input$data_col,
           title = "Cleaned Data", subtitle = subtitle) +
      fmd_theme()

    if (length(lines) > 0) {
      p <- p + geom_vline(xintercept = lines, color = "red", linetype = "dashed")
    }

    if (meth == "butterworth") {
      p <- p +
        geom_point(data = working(), aes(x = time, y = data),
                   size = 0.5, color = "grey75", alpha = 0.5) +
        geom_point(size = 0.7, color = "steelblue", alpha = 0.8)
    } else {
      p <- p +
        geom_point(aes(color = outlier), size = 0.7, alpha = 0.75) +
        scale_color_manual(
          values = c("FALSE" = "steelblue", "TRUE" = "tomato"),
          labels = c("FALSE" = "Kept", "TRUE" = "Interpolated (PCHIP)"),
          name   = NULL
        )
    }
    p
  })

  output$download_data <- downloadHandler(
    filename = function() {
      base <- tools::file_path_sans_ext(input$file1$name)
      paste0("cleaned_", base, "_", Sys.Date(), ".csv")
    },
    content = function(file) {
      df          <- cleaned_data()
      df$filename <- tools::file_path_sans_ext(input$file1$name)
      df$method   <- input$method
      write.csv(df, file, row.names = FALSE)
    }
  )
}

shinyApp(ui = ui, server = server)