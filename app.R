library(shiny)
library(bslib)
library(readxl)
library(dplyr)
library(tidyr)
library(stringr)
library(purrr)
library(DT)
library(ggplot2)
library(scales)

# ---------- Helpers ----------

clean_category <- function(x) {
  x %>%
    str_squish() %>%
    str_replace("^Who are the top 3 on playmakers\\?$", "Who are the top 3 playmakers?")
}

short_category <- function(x) {
  case_when(
    str_detect(x, "team captain") ~ "Draft Picks",
    str_detect(x, "pure scorers") ~ "Scorers",
    str_detect(x, "on-ball defenders") ~ "On-Ball Defense",
    str_detect(x, "playmakers") ~ "Playmakers",
    str_detect(x, "shooters") ~ "Shooters",
    str_detect(x, "team defenders") ~ "Team Defense",
    str_detect(x, "DON'T want") ~ "Avoid List",
    str_detect(x, "Bball IQ") ~ "Basketball IQ",
    str_detect(x, "raw athletes") ~ "Athletes",
    TRUE ~ x
  )
}

parse_poll <- function(path) {
  raw <- read_excel(path, sheet = 1, .name_repair = "minimal")

  if (ncol(raw) < 2) {
    stop("The workbook does not appear to contain poll-response columns.")
  }

  headers <- names(raw)
  keep <- str_detect(headers, "\\[[^]]+\\]\\s*$")
  keep[1] <- FALSE

  if (!any(keep)) {
    stop("No columns ending in [Player Name] were found. Please upload the original Google Forms-style export.")
  }

  response_cols <- raw[, keep, drop = FALSE]
  original_names <- names(response_cols)

  meta <- tibble(
    column = original_names,
    category = clean_category(str_remove(original_names, "\\s*\\[[^]]+\\]\\s*$")),
    player = str_match(original_names, "\\[([^]]+)\\]\\s*$")[, 2]
  ) %>%
    filter(!str_detect(player, "^Row \\d+$"))

  valid_cols <- meta$column
  response_cols <- response_cols[, valid_cols, drop = FALSE]

  long <- response_cols %>%
    mutate(response_id = row_number()) %>%
    pivot_longer(-response_id, names_to = "column", values_to = "vote") %>%
    left_join(meta, by = "column") %>%
    mutate(
      vote = suppressWarnings(as.integer(vote)),
      category_short = map_chr(category, short_category)
    ) %>%
    filter(vote %in% 1:3, !is.na(player), player != "") %>%
    select(response_id, category, category_short, player, vote)

  if (nrow(long) == 0) {
    stop("The poll columns were found, but no votes coded 1, 2, or 3 were detected.")
  }

  list(raw = raw, long = long)
}

rankings_from_votes <- function(long, w1 = 3, w2 = 2, w3 = 1) {
  all_players <- long %>%
    distinct(category, category_short, player)

  counts <- long %>%
    count(category, category_short, player, vote, name = "n") %>%
    mutate(vote = paste0("vote", vote)) %>%
    pivot_wider(names_from = vote, values_from = n, values_fill = 0)

  all_players %>%
    left_join(counts, by = c("category", "category_short", "player")) %>%
    mutate(
      across(c(vote1, vote2, vote3), ~replace_na(.x, 0L)),
      total_votes = vote1 + vote2 + vote3,
      weighted_score = vote1 * w1 + vote2 * w2 + vote3 * w3
    ) %>%
    group_by(category, category_short) %>%
    arrange(desc(weighted_score), desc(vote1), desc(vote2), desc(vote3), player, .by_group = TRUE) %>%
    mutate(
      rank = min_rank(desc(weighted_score)),
      order = row_number()
    ) %>%
    ungroup()
}

safe_pct <- function(x, denom) ifelse(denom > 0, x / denom, 0)

# ---------- UI ----------

ui <- page_navbar(
  title = "Good Run Basketball Rankings",
  theme = bs_theme(version = 5, bootswatch = "flatly", primary = "#244C66"),
  header = tagList(
    tags$style(HTML("\n      .rank-1 {font-weight:700;}\n      .top-three {background-color: rgba(36,76,102,.08) !important;}\n      .small-note {font-size:.88rem; color:#6c757d;}\n      .value-box .value-box-value {font-size:2rem;}\n      .shiny-output-error-validation {color:#8a2c2c;}\n    "))
  ),

  nav_panel(
    "Rankings",
    layout_sidebar(
      sidebar = sidebar(
        fileInput("file", "Upload a new poll export", accept = c(".xlsx", ".xls")),
        p(class = "small-note", "If no file is uploaded, the included sample_poll.xlsx is used."),
        selectInput("category", "Category", choices = NULL),
        hr(),
        h5("Scoring"),
        numericInput("w1", "1st-place points", value = 3, min = 0, step = 1),
        numericInput("w2", "2nd-place points", value = 2, min = 0, step = 1),
        numericInput("w3", "3rd-place points", value = 1, min = 0, step = 1),
        actionButton("reset_weights", "Reset to 3–2–1", class = "btn-outline-secondary w-100"),
        hr(),
        downloadButton("download_rankings", "Download current rankings", class = "w-100")
      ),
      card(
        card_header(textOutput("category_question")),
        uiOutput("ranking_metrics"),
        DTOutput("ranking_table")
      )
    )
  ),

  nav_panel(
    "Compare Players",
    layout_sidebar(
      sidebar = sidebar(
        selectizeInput("compare_players", "Players", choices = NULL, multiple = TRUE,
                       options = list(maxItems = 5, placeholder = "Choose 2–5 players")),
        checkboxInput("compare_hide_avoid", "Exclude the ‘Avoid List’ category", value = FALSE)
      ),
      card(
        card_header("Head-to-head category comparison"),
        plotOutput("compare_plot", height = 430),
        DTOutput("compare_table")
      )
    )
  ),

  nav_panel(
    "Player Profile",
    layout_sidebar(
      sidebar = sidebar(
        selectizeInput("profile_player", "Player", choices = NULL, multiple = FALSE)
      ),
      card(
        card_header(uiOutput("profile_header")),
        uiOutput("profile_metrics"),
        plotOutput("profile_plot", height = 420),
        DTOutput("profile_table")
      )
    )
  ),

  nav_panel(
    "About",
    card(
      card_header("How scoring works"),
      p("The app reads the original Google Forms-style Excel export. Each poll column must end with a player name in brackets, for example: ‘Who are the top 3 shooters? [Ramal]’."),
      p("Votes coded 1, 2, and 3 are interpreted as first-, second-, and third-place selections. The default weights are 3, 2, and 1 points, and can be changed interactively."),
      p("Ranks are based on weighted score. Display order breaks score ties using more first-place votes, then more second-place votes, then more third-place votes, then player name. The displayed rank itself remains tied when weighted scores are tied."),
      p("Columns such as [Row 32], [Row 33], and [Row 34] are ignored automatically."),
      hr(),
      p(class = "small-note", "Built as a single-file R Shiny application. Required packages: shiny, bslib, readxl, dplyr, tidyr, stringr, purrr, DT, ggplot2, scales.")
    )
  )
)

# ---------- Server ----------

server <- function(input, output, session) {

  observeEvent(input$reset_weights, {
    updateNumericInput(session, "w1", value = 3)
    updateNumericInput(session, "w2", value = 2)
    updateNumericInput(session, "w3", value = 1)
  })

  poll <- reactive({
    path <- if (!is.null(input$file)) input$file$datapath else "sample_poll.xlsx"
    validate(need(file.exists(path), "sample_poll.xlsx was not found. Upload the poll workbook to continue."))
    tryCatch(
      parse_poll(path),
      error = function(e) validate(need(FALSE, e$message))
    )
  })

  rankings <- reactive({
    req(poll())
    req(input$w1, input$w2, input$w3)
    rankings_from_votes(poll()$long, input$w1, input$w2, input$w3)
  })

  observeEvent(poll(), {
    cats <- poll()$long %>% distinct(category_short, category)
    choices <- setNames(cats$category, cats$category_short)
    updateSelectInput(session, "category", choices = choices,
                      selected = if ("Draft Picks" %in% names(choices)) choices[["Draft Picks"]] else choices[[1]])

    players <- sort(unique(poll()$long$player))
    default_compare <- intersect(c("Cyrus", "Taraz", "Ramal"), players)
    if (length(default_compare) < 2) default_compare <- head(players, min(3, length(players)))
    updateSelectizeInput(session, "compare_players", choices = players, selected = default_compare, server = TRUE)
    updateSelectizeInput(session, "profile_player", choices = players,
                         selected = if ("Taraz" %in% players) "Taraz" else players[[1]], server = TRUE)
  }, ignoreInit = FALSE)

  selected_ranking <- reactive({
    req(input$category)
    rankings() %>%
      filter(category == input$category) %>%
      arrange(desc(weighted_score), desc(vote1), desc(vote2), desc(vote3), player)
  })

  output$category_question <- renderText({
    req(input$category)
    input$category
  })

  output$ranking_metrics <- renderUI({
    dat <- selected_ranking()
    req(nrow(dat) > 0)
    top <- dat %>% slice(1)
    n_ballots <- poll()$long %>% filter(category == input$category) %>% distinct(response_id) %>% nrow()

    layout_columns(
      col_widths = c(4, 4, 4),
      value_box("Leader", top$player, paste0(top$weighted_score, " points"), showcase = bsicons::bs_icon("trophy")),
      value_box("Ballots with a vote", n_ballots, "in this category", showcase = bsicons::bs_icon("people")),
      value_box("Scoring", paste(input$w1, input$w2, input$w3, sep = "–"), "1st – 2nd – 3rd", showcase = bsicons::bs_icon("123"))
    )
  })

  output$ranking_table <- renderDT({
    dat <- selected_ranking() %>%
      transmute(
        Rank = rank,
        Player = player,
        `1st` = vote1,
        `2nd` = vote2,
        `3rd` = vote3,
        `Total votes` = total_votes,
        `Weighted score` = weighted_score
      )

    datatable(
      dat,
      rownames = FALSE,
      extensions = "Buttons",
      options = list(
        pageLength = 32,
        lengthMenu = c(10, 20, 32, 50),
        order = list(list(0, "asc"), list(6, "desc")),
        dom = "Bfrtip",
        buttons = c("copy", "csv"),
        columnDefs = list(list(className = "dt-center", targets = c(0, 2, 3, 4, 5, 6)))
      ),
      class = "stripe hover compact"
    ) %>%
      formatStyle("Rank", target = "row", backgroundColor = styleEqual(c(1, 2, 3), c("rgba(218,165,32,.18)", "rgba(160,160,160,.12)", "rgba(176,112,61,.12)"))) %>%
      formatStyle("Weighted score", fontWeight = "bold")
  })

  output$download_rankings <- downloadHandler(
    filename = function() paste0("basketball_rankings_", Sys.Date(), ".csv"),
    content = function(file) {
      out <- rankings() %>%
        transmute(
          Category = category_short,
          Question = category,
          Rank = rank,
          Player = player,
          First = vote1,
          Second = vote2,
          Third = vote3,
          TotalVotes = total_votes,
          WeightedScore = weighted_score
        )
      write.csv(out, file, row.names = FALSE)
    }
  )

  compare_data <- reactive({
    req(length(input$compare_players) >= 1)
    dat <- rankings() %>% filter(player %in% input$compare_players)
    if (isTRUE(input$compare_hide_avoid)) dat <- dat %>% filter(category_short != "Avoid List")
    dat
  })

  output$compare_plot <- renderPlot({
    dat <- compare_data()
    validate(need(nrow(dat) > 0, "Choose at least one player."))

    ggplot(dat, aes(x = category_short, y = rank, group = player, linetype = player, shape = player)) +
      geom_line(linewidth = 0.7) +
      geom_point(size = 3) +
      scale_y_reverse(breaks = pretty_breaks()) +
      labs(x = NULL, y = "Rank (1 is best)", linetype = "Player", shape = "Player") +
      theme_minimal(base_size = 12) +
      theme(axis.text.x = element_text(angle = 35, hjust = 1), legend.position = "bottom")
  })

  output$compare_table <- renderDT({
    dat <- compare_data() %>%
      select(Category = category_short, Player = player, Rank = rank, `Weighted score` = weighted_score) %>%
      arrange(Category, Rank, desc(`Weighted score`))
    datatable(dat, rownames = FALSE, options = list(pageLength = 20, dom = "tip"), class = "stripe hover compact")
  })

  profile_data <- reactive({
    req(input$profile_player)
    rankings() %>% filter(player == input$profile_player) %>% arrange(rank, category_short)
  })

  output$profile_header <- renderUI({
    req(input$profile_player)
    tags$span(tags$strong(input$profile_player), " — category profile")
  })

  output$profile_metrics <- renderUI({
    dat <- profile_data()
    req(nrow(dat) > 0)
    best <- dat %>% arrange(rank, desc(weighted_score)) %>% slice(1)
    first_votes <- sum(dat$vote1)
    avg_rank <- mean(dat$rank)

    layout_columns(
      col_widths = c(4, 4, 4),
      value_box("Best category", best$category_short, paste0("Rank #", best$rank)),
      value_box("First-place votes", first_votes, "across all categories"),
      value_box("Average rank", sprintf("%.1f", avg_rank), "across all categories")
    )
  })

  output$profile_plot <- renderPlot({
    dat <- profile_data() %>% arrange(rank) %>% mutate(category_short = factor(category_short, levels = rev(category_short)))

    ggplot(dat, aes(x = rank, y = category_short)) +
      geom_segment(aes(x = max(rank, na.rm = TRUE) + 1, xend = rank, yend = category_short), linewidth = 2, alpha = 0.35) +
      geom_point(size = 4) +
      scale_x_reverse(breaks = pretty_breaks()) +
      labs(x = "Rank (1 is best)", y = NULL) +
      theme_minimal(base_size = 12)
  })

  output$profile_table <- renderDT({
    dat <- profile_data() %>%
      transmute(Category = category_short, Rank = rank, `Weighted score` = weighted_score,
                `1st` = vote1, `2nd` = vote2, `3rd` = vote3, `Total votes` = total_votes)
    datatable(dat, rownames = FALSE, options = list(pageLength = 10, dom = "tip"), class = "stripe hover compact")
  })
}

shinyApp(ui, server)
