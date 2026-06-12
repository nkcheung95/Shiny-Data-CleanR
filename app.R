# FMD DIAMETER CLEANER v0.2
# Packages
packages <- c("tidyverse", "zoo", "scales", "ggplot2", "shiny", "signal", "readxl", "ragg")

install_load_packages <- function(packages) {
  not_installed <- setdiff(packages, rownames(installed.packages()))
  if (length(not_installed) > 0) install.packages(not_installed, repos = "https://cloud.r-project.org")
  invisible(sapply(packages, library, character.only = TRUE))
}
install_load_packages(packages)


# ── Helpers ────────────────────────────────────────────────────────────────────

# PCHIP-equivalent: monotone Hermite spline interpolation over NA gaps
pchip_interp <- function(x) {
  idx   <- seq_along(x)
  valid <- !is.na(x)
  if (sum(valid) < 2) return(x)
  f       <- splinefun(idx[valid], x[valid], method = "monoH.FC")
  x[!valid] <- f(idx[!valid])
  x
}

# Reflect-pad a vector by n samples on each side (avoids edge artefacts)
reflect_pad <- function(x, n) {
  n <- min(n, length(x) - 1)
  c(rev(x[2:(n + 1)]), x, rev(x[(length(x) - n):(length(x) - 1)]))
}

unpad <- function(x, n) x[(n + 1):(length(x) - n)]

# Auto x-axis break interval based on total duration
smart_breaks <- function(x_max) {
  interval <- dplyr::case_when(
    x_max <= 120  ~ 10,
    x_max <= 600  ~ 30,
    x_max <= 1800 ~ 60,
    x_max <= 7200 ~ 300,
    TRUE          ~ 600
  )
  seq(0, ceiling(x_max / interval) * interval, by = interval)
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
  x        <- pchip_interp(df$data)           # fill any NAs before filtering
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


# ── File loader ────────────────────────────────────────────────────────────────

load_file <- function(path, ext, delimiter) {
  if (ext %in% c("xlsx", "xls")) {
    read_excel(path, col_names = FALSE)
  } else {
    sep <- switch(delimiter, Tab = "\t", Comma = ",", Semicolon = ";", "\t")
    read.delim(path, sep = sep, header = FALSE, stringsAsFactors = FALSE)
  }
}


# ── UI ─────────────────────────────────────────────────────────────────────────

ui <- fluidPage(
  titlePanel("DATA CLEANER v0.2"),

  sidebarLayout(
    sidebarPanel(
      width = 3,

      h4("File"),
      fileInput("file1", "CSV / TXT / XLSX",
                accept = c(".csv", ".txt", ".xlsx", ".xls")),
      selectInput("delimiter", "Text delimiter",
                  choices = c(Tab = "Tab", Comma = "Comma", Semicolon = "Semicolon"),
                  selected = "Tab"),
      numericInput("skip_rows", "Header rows to skip", value = 8, min = 0, max = 100),

      hr(),
      h4("Columns"),
      uiOutput("column_selectors"),

      hr(),
      h4("Display"),
      checkboxInput("show_vlines", "Show FMD lines (60 s & 360 s)", value = TRUE),

      hr(),
      downloadButton("download_data", "Download Cleaned Data")
    ),

    mainPanel(
      width = 9,

      # ── Method + controls ──
      wellPanel(
        h4("Smoothing Method"),
        radioButtons("method", NULL,
          choices = c(
            "Median + IQR"                     = "median_iqr",
            "Butterworth Low-Pass"             = "butterworth",
            "Median + IQR  →  Butterworth"     = "combined"
          ),
          inline = TRUE
        ),

        fluidRow(
          # Median / IQR controls
          conditionalPanel(
            condition = "input.method == 'median_iqr' || input.method == 'combined'",
            column(4,
              h5("Median + IQR"),
              sliderInput("window_size", "Window size (odd samples)",
                          min = 3, max = 501, value = 31, step = 2, ticks = FALSE),
              sliderInput("iqr_threshold", "IQR threshold multiplier",
                          min = 0, max = 5, value = 1.5, step = 0.25, ticks = FALSE)
            )
          ),
          # Butterworth controls
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
            helpText(
              strong("PCHIP"), "— monotone piecewise cubic Hermite.",
              "Preserves shape and avoids oscillation near gaps.",
              "Applied to all outlier / NA replacement."
            )
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

  raw_data <- reactive({
    req(input$file1)
    ext <- tolower(tools::file_ext(input$file1$name))
    load_file(input$file1$datapath, ext, input$delimiter)
  })

  trimmed_data <- reactive({
    req(raw_data())
    skip <- max(0L, as.integer(input$skip_rows))
    df   <- raw_data()
    if (skip >= nrow(df)) return(df)
    df[(skip + 1):nrow(df), ]
  })

  output$column_selectors <- renderUI({
    req(trimmed_data())
    cols <- colnames(trimmed_data())
    tagList(
      selectInput("time_col", "Time column",   choices = cols, selected = cols[1]),
      selectInput("data_col", "Data column",   choices = cols, selected = cols[min(2, length(cols))]),
      selectInput("comments_col", "Comments column (optional)",
                  choices = c("None", cols),
                  selected = if (length(cols) >= 3) cols[3] else "None")
    )
  })

  working <- reactive({
    req(trimmed_data(), input$time_col, input$data_col)
    df <- trimmed_data()
    df <- rename(df, time = !!input$time_col, data = !!input$data_col)
    if (!is.null(input$comments_col) && input$comments_col != "None")
      df <- rename(df, comments = !!input$comments_col)
    df$data <- suppressWarnings(as.numeric(as.character(df$data)))
    df$time <- suppressWarnings(as.numeric(as.character(df$time)))
    df <- df[!is.na(df$time) & !is.na(df$data), ]
    df$time <- df$time - min(df$time)
    df
  })

  sample_rate <- reactive({
    req(working())
    dt <- median(diff(working()$time), na.rm = TRUE)
    if (is.na(dt) || dt <= 0) return(30)
    1 / dt
  })

  output$cutoff_slider <- renderUI({
    nyq     <- sample_rate() / 2
    max_cut <- min(30, round(nyq * 0.95, 3))
    def_cut <- min(1.0, max_cut)
    sliderInput("butter_cutoff", "Cutoff frequency (Hz)",
                min = 0.001, max = max_cut, value = def_cut,
                step = 0.001, ticks = FALSE)
  })

  output$file_info <- renderPrint({
    df <- working()
    cat(sprintf(
      "Rows: %d  |  Time: %.1f – %.1f s  |  Sample rate ≈ %.2f Hz\n",
      nrow(df), min(df$time), max(df$time), sample_rate()
    ))
  })

  # ── Base plot theme ──
  fmd_theme <- function() {
    theme_minimal(base_size = 13) +
      theme(legend.position = c(0.88, 0.15),
            legend.background = element_rect(fill = alpha("white", 0.7), color = NA))
  }

  output$raw_plot <- renderPlot({
    req(working())
    df <- working()
    ggplot(df, aes(x = time, y = data)) +
      geom_point(size = 0.7, color = "steelblue", alpha = 0.6) +
      scale_x_continuous(breaks = smart_breaks(max(df$time, na.rm = TRUE)), labels = comma) +
      labs(x = "Time (seconds)", y = input$data_col, title = "Raw Data") +
      { if (input$show_vlines)
          geom_vline(xintercept = c(60, 360), color = "red", linetype = "dashed") } +
      fmd_theme()
  })

  cleaned_data <- reactive({
    req(working(), input$method)
    sr <- sample_rate()
    tryCatch(
      switch(input$method,
        median_iqr  = clean_median_iqr(working(), input$window_size, input$iqr_threshold),
        butterworth = clean_butterworth(working(), sr, input$butter_cutoff, input$butter_order),
        combined    = clean_combined(working(), input$window_size, input$iqr_threshold,
                                     sr, input$butter_cutoff, input$butter_order)
      ),
      error = function(e) {
        showNotification(paste("Error:", e$message), type = "error", duration = 8)
        NULL
      }
    )
  })

  output$clean_plot <- renderPlot({
    req(cleaned_data())
    df    <- cleaned_data()
    meth  <- input$method

    subtitle <- switch(meth,
      median_iqr  = paste0("Median+IQR  |  window = ", input$window_size,
                           " samples,  threshold = ", input$iqr_threshold, " × IQR"),
      butterworth = paste0("Butterworth  |  cutoff = ", input$butter_cutoff,
                           " Hz,  order = ", input$butter_order),
      combined    = paste0("Median+IQR (w=", input$window_size, ", t=", input$iqr_threshold,
                           ")  →  Butterworth (", input$butter_cutoff,
                           " Hz, order ", input$butter_order, ")")
    )

    p <- ggplot(df, aes(x = time, y = clean_data)) +
      scale_x_continuous(breaks = smart_breaks(max(df$time, na.rm = TRUE)), labels = comma) +
      labs(x = "Time (seconds)", y = input$data_col,
           title = "Cleaned Data", subtitle = subtitle) +
      { if (input$show_vlines)
          geom_vline(xintercept = c(60, 360), color = "red", linetype = "dashed") } +
      fmd_theme()

    if (meth == "butterworth") {
      # Raw in grey underneath, filtered in blue on top
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
    content  = function(file) {
      df          <- cleaned_data()
      df$filename <- tools::file_path_sans_ext(input$file1$name)
      df$method   <- input$method
      write.csv(df, file, row.names = FALSE)
    }
  )
}

shinyApp(ui = ui, server = server)