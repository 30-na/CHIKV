library(shiny)
library(ggplot2)

if (file.exists("../R/thermal_functions.R")) {
  source("../R/thermal_functions.R")
} else {
  source("thermal_functions.R")
}

temp_default <- c(15, 36)

function_sets <- list(
  aegypti = list(
    label = "Ae. aegypti",
    a = list(
      "Mordecai 2017" = a_aegypti_mordecai_2017,
      "Scott 2000" = a_aegypti_scott_2000
    ),
    b = list(
      "Mordecai 2017" = b_aegypti_mordecai_2017,
      "Lambrechts 2011" = b_aegypti_lambrechts_2011
    ),
    c = list(
      "Mordecai 2017" = c_aegypti_mordecai_2017,
      "Lambrechts 2011" = c_aegypti_lambrechts_2011
    ),
    lf = list(
      "Mordecai 2017" = lf_aegypti_mordecai_2017,
      "Brady 2013" = lf_aegypti_brady_2013,
      "Yang 2009" = lf_aegypti_yang_2009
    ),
    PDR = list(
      "Mordecai 2017" = PDR_aegypti_mordecai_2017,
      "Focks 1995" = PDR_aegypti_focks_1995
    ),
    EFD = list(
      "Mordecai 2017" = EFD_aegypti_mordecai_2017,
      "Yang 2009" = EFD_aegypti_yang_2009
    ),
    pEA = list(
      "Mordecai 2017" = pEA_aegypti_mordecai_2017
    ),
    MDR = list(
      "Mordecai 2017" = MDR_aegypti_mordecai_2017,
      "Yang 2009" = phi_v_aegypti_yang_2009
    )
  ),
  albopictus = list(
    label = "Ae. albopictus",
    a = list(
      "Mordecai 2017" = a_albopictus_mordecai_2017,
      "Scott 2000" = a_albopictus_scott_2000
    ),
    b = list(
      "Mordecai 2017" = b_albopictus_mordecai_2017
    ),
    c = list(
      "Mordecai 2017" = c_albopictus_mordecai_2017
    ),
    lf = list(
      "Mordecai 2017" = lf_albopictus_mordecai_2017,
      "Poletti 2011" = lf_albopictus_poletti_2011,
      "Ruiz-Moreno 2012" = lf_albopictus_ruiz_moreno_2012,
      "Brady 2013" = lf_albopictus_brady_2013,
      "Ng 2017" = lf_albopictus_ng_2017
    ),
    PDR = list(
      "Mordecai 2017" = PDR_albopictus_mordecai_2017,
      "Chan & Johansson 2012" = PDR_albopictus_chan_johansson_2012,
      "Chan & Johansson 2012 alt" = PDR_albopictus_chan_johansson_2012_alt,
      "Ruiz-Moreno 2012" = PDR_albopictus_ruiz_moreno_2012
    ),
    EFD = list(
      "Mordecai 2017" = TFD_albopictus_mordecai_2017
    ),
    pEA = list(
      "Mordecai 2017" = pEA_albopictus_mordecai_2017,
      "Yang 2009 converted" = pEA_albopictus_yang_2009,
      "Poletti 2011 converted" = pEA_albopictus_poletti_2011
    ),
    MDR = list(
      "Mordecai 2017" = MDR_albopictus_mordecai_2017,
      "Poletti 2011 converted" = MDR_albopictus_poletti_2011
    )
  )
)

trait_ids <- c("a", "b", "c", "lf", "PDR", "EFD", "pEA", "MDR")

baseline_choices <- list(
  a = "Mordecai 2017",
  b = "Mordecai 2017",
  c = "Mordecai 2017",
  lf = "Mordecai 2017",
  PDR = "Mordecai 2017",
  EFD = "Mordecai 2017",
  pEA = "Mordecai 2017",
  MDR = "Mordecai 2017"
)

calculate_R0 <- function(temp, species_set, choices, N, r) {
  lf <- species_set$lf[[choices$lf]](temp)
  mu <- 1 / lf

  R0 <- relative_R0_mordecai(
    a = species_set$a[[choices$a]](temp),
    b = species_set$b[[choices$b]](temp),
    c = species_set$c[[choices$c]](temp),
    mu = mu,
    PDR = species_set$PDR[[choices$PDR]](temp),
    EFD = species_set$EFD[[choices$EFD]](temp),
    pEA = species_set$pEA[[choices$pEA]](temp),
    MDR = species_set$MDR[[choices$MDR]](temp),
    N = N,
    r = r
  )

  R0[!is.finite(R0)] <- NA
  return(R0)
}

all_combination_max_R0 <- function(temp, species_set, N, r) {
  combinations <- expand.grid(
    a = names(species_set$a),
    b = names(species_set$b),
    c = names(species_set$c),
    lf = names(species_set$lf),
    PDR = names(species_set$PDR),
    EFD = names(species_set$EFD),
    pEA = names(species_set$pEA),
    MDR = names(species_set$MDR),
    stringsAsFactors = FALSE
  )

  max_R0 <- NA_real_

  for (i in seq_len(nrow(combinations))) {
    choices <- as.list(combinations[i, ])
    R0 <- calculate_R0(temp, species_set, choices, N, r)
    current_max <- suppressWarnings(max(R0, na.rm = TRUE))

    if (is.finite(current_max) && (is.na(max_R0) || current_max > max_R0)) {
      max_R0 <- current_max
    }
  }

  return(max_R0)
}

ui <- fluidPage(
  titlePanel("CHIKV R0 Explorer"),
  sidebarLayout(
    sidebarPanel(
      selectInput("species", "Mosquito species", choices = c("Ae. aegypti" = "aegypti", "Ae. albopictus" = "albopictus")),
      numericInput("temp_min", "Min temp (C)", value = temp_default[1], min = -10, max = 50, step = 0.1),
      numericInput("temp_max", "Max temp (C)", value = temp_default[2], min = -10, max = 50, step = 0.1),
      selectInput("a", "Biting rate, a(T)", choices = NULL),
      selectInput("b", "Vector-to-human transmission, b(T)", choices = NULL),
      selectInput("c", "Human-to-vector transmission, c(T)", choices = NULL),
      selectInput("lf", "Adult mosquito lifespan, 1/mu(T)", choices = NULL),
      selectInput("PDR", "Extrinsic incubation / PDR(T)", choices = NULL),
      selectInput("EFD", "Fecundity, EFD/TFD(T)", choices = NULL),
      selectInput("pEA", "Egg-adult survival, pEA(T)", choices = NULL),
      selectInput("MDR", "Aquatic maturation, MDR(T)", choices = NULL)
    ),
    mainPanel(
      plotOutput("r0_plot", height = "520px"),
      verbatimTextOutput("summary")
    )
  )
)

server <- function(input, output, session) {

  observeEvent(input$species, {
    species_set <- function_sets[[input$species]]

    for (id in trait_ids) {
      choices <- names(species_set[[id]])
      updateSelectInput(session, id, choices = choices, selected = choices[1])
    }
  }, ignoreInit = FALSE)

  r0_data <- reactive({
    species_set <- function_sets[[input$species]]
    temp <- seq(min(input$temp_min, input$temp_max), max(input$temp_min, input$temp_max), by = 0.1)

    selected_choices <- list(
      a = input$a,
      b = input$b,
      c = input$c,
      lf = input$lf,
      PDR = input$PDR,
      EFD = input$EFD,
      pEA = input$pEA,
      MDR = input$MDR
    )

    selected_R0 <- calculate_R0(temp, species_set, selected_choices, N = 1, r = 1)
    baseline_R0 <- calculate_R0(temp, species_set, baseline_choices, N = 1, r = 1)
    max_R0 <- all_combination_max_R0(temp, species_set, N = 1, r = 1)

    data.frame(
      temp = rep(temp, 2),
      R0 = c(selected_R0 / max_R0, baseline_R0 / max_R0),
      curve = rep(c("Selected functions", "Mordecai baseline"), each = length(temp))
    )
  })

  output$r0_plot <- renderPlot({
    data <- r0_data()
    species_label <- function_sets[[input$species]]$label

    ggplot(data, aes(x = temp, y = R0, color = curve, linetype = curve)) +
      geom_line(linewidth = 1) +
      coord_cartesian(ylim = c(0, 1)) +
      scale_color_manual(values = c(
        "Selected functions" = if (input$species == "aegypti") "#1f78b4" else "#33a02c",
        "Mordecai baseline" = "#555555"
      )) +
      scale_linetype_manual(values = c(
        "Selected functions" = "solid",
        "Mordecai baseline" = "dashed"
      )) +
      labs(
        x = "Temperature (C)",
        y = "Standardized relative R0(T)",
        color = "Curve",
        linetype = "Curve",
        title = paste("Standardized R0 thermal function for", species_label)
      ) +
      theme_bw()
  })

  output$summary <- renderText({
    data <- r0_data()
    selected_data <- data[data$curve == "Selected functions", ]
    finite_data <- selected_data[is.finite(selected_data$R0), ]

    if (nrow(finite_data) == 0) {
      return("No finite R0 values in the selected temperature range.")
    }

    peak <- finite_data[which.max(finite_data$R0), ]
    paste0(
      "Peak standardized R0 = ", round(peak$R0, 3), " at ", round(peak$temp, 1), " C"
    )
  })
}

shinyApp(ui, server)
