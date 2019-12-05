Sys.setenv(TZ = 'UTC')

library(ggvis)
library(dplyr)

if (FALSE) {
  library(RSQLite)
  library(dbplyr)
}

#
# Load matchup data
#

load("m_data_p.RData")
load("m_data_b.RData")

function(input, output, session) {

  # Filter the data, returning a data frame
  olci_matchups <- reactive({
    spatial <- input$olci_spt
    if(spatial == "Center pixel") {
      m_data <- m_data_p
    } else {
      m_data <- m_data_b
    }

    mindate <- input$olci_trng[1]
    maxdate <- input$olci_trng[2]
    maxtime <- input$olci_maxt
    minvz   <- input$olci_vza[1]
    maxvz   <- input$olci_vza[2]
    minsz   <- input$olci_sza[1]
    maxsz   <- input$olci_sza[2]
    minaot  <- input$olci_aot[1]
    maxaot  <- input$olci_aot[2]

    # Apply filters
    m <- m_data %>%
      filter(
        as.Date(olci_time) >= mindate,
        as.Date(olci_time) <= maxdate,
        abs(olci_dift) <= maxtime,
        olci_oza >= minvz,
        olci_oza <= maxvz,
        olci_sza >= minsz,
        olci_sza <= maxsz,
        olci_aot865 >= minaot,
        olci_aot865 <= maxaot
      )

    # Optional: filter by dataset
    if (input$olci_dtst != "All") {
      datasource <- switch(input$olci_dtst,
                           "LifeWatch BE" = "lw_st_be",
                           "BMDC" = "bmdc_st_be",
                           "AERONET-OC" = "aeronetoc_be",
                           "CEFAS SmartBuoys" = "cefasbuoy_uk"
                    )
      m <- m %>% filter(Dataset == datasource)
    }

    # In case multiple observations exist for a given location per day, 
    # keep only the closest in time to overpass:
    cstp <- paste(format(m$Time, "%Y%m%d"), m$Dataset, m$Station, sep = "_")
    ustp <- unique(cstp)
    m_u  <- NULL
    for(k in 1:length(ustp)) {
      id  <- which(cstp == ustp[k])
      m_u <- rbind(m_u, m[id[order(abs(m[id, ]$olci_dift))], ][1, ])
    }
    m <- m_u

    m <- as.data.frame(m)

    m
  })

  # Function for generating tooltip text
  matchup_tooltip <- function(x) {
    if (is.null(x)) return(NULL)
    if (is.null(x$ID)) return(NULL)

    # Pick out the data point with this ID
    all_matchups <- isolate(olci_matchups())
    matchup      <- all_matchups[all_matchups$ID == x$ID, ]

    visn <- paste(matchup$Station, format(matchup$Time, "%H%M%S"), 
            matchup$olci_tile, sep = "_") %>%
            paste0(., ".png")

    paste0("<b>", paste(matchup$Dataset, matchup$Station, sep = ": "), "</b><br>",
      matchup$olci_time, "<br>",
      paste("Time dif.:", round(matchup$olci_dift, 2), "hours<br>"),
      paste("AOT(865):", round(matchup$olci_aot865, 2), "<br>"),
      paste0("R<sub>rs</sub>(865): ", round(matchup$olci_Rrs865, 4), "&plusmn;", 
        round(matchup$olci_Rrs865_er, 4), " sr<br>"),
      paste0("VZA: ", round(matchup$olci_oza, 2), "°<br>"),
      paste0("SZA: ", round(matchup$olci_sza, 2), "°<br>"),
      matchup$olci_tile,
      tags$p(tags$img(height = 300, width = 300, src = visn))
    )
  }

  # A reactive expression with the ggvis plot
  vis <- reactive({

    # Get requested data:
    olcip <- get_olci_prod(input$olci_prd)
    instp <- get_olci_insitu_prod(input$olci_prd)
    yvar  <- prop("y", as.symbol(olcip))
    xvar  <- prop("x", as.symbol(instp))

    # Labels for axes:
    xylab <- get_olci_xylab(input$olci_prd)
    xvar_name <- paste("In situ", gsub("\\(NN\\)|\\(OC4Me\\)", "", 
      input$olci_prd), xylab)
    yvar_name <- paste("OLCI", input$olci_prd, xylab)

    # Remove NA and check requisites:
    m  <- olci_matchups()
    req(!is.null(m))

    id <- m[, c(instp, olcip)] %>%
          na.omit() %>%
          attr("na.action")
    req((nrow(m) - length(id)) > 2)
    if(length(id) > 0) m <- m[-id, ]

    # Make plot:
    data_line <- data.frame(
      x_rng = c(0, range(m[, instp], m[, olcip])),
      y_rng = c(0, range(m[, instp], m[, olcip]))
    )

    m %>%
    ggvis(x = xvar, y = yvar) %>%
    layer_points(size := 50, size.hover := 200,
      fillOpacity := 0.2, fillOpacity.hover := 0.5, key := ~ID) %>%
    add_tooltip(matchup_tooltip, "hover") %>%
    add_axis("x", title = xvar_name, title_offset = 50) %>%
    add_axis("y", title = yvar_name, title_offset = 50) %>%
    layer_paths(x = ~x_rng, y = ~y_rng, data = data_line) %>%
    layer_model_predictions(model = "MASS::rlm", se = FALSE) %>%
    set_options(width = 550, height = 550)
  })

  vis %>% bind_shiny("plot1")

  output$olci_stats <- renderUI({
    olcip <- get_olci_prod(input$olci_prd)
    instp <- get_olci_insitu_prod(input$olci_prd)
    xylab <- get_olci_xylab(input$olci_prd)

    m  <- olci_matchups()
    if(is.null(m)) 
      return(HTML(paste("<p, style ='color:#FF0000'>Less than 2 data points.",
        "Please relax the filter criteria. </p>")))

    id <- m[, c(instp, olcip)] %>%
          na.omit() %>%
          attr("na.action")
    if((nrow(m) - length(id)) < 2) 
       return(HTML(paste("<p, style ='color:#FF0000'>Less than 2 data points.",
        "Please relax the filter criteria. </p>")))

    if(length(id) > 0) m <- m[-id, ]

    x <- m[, instp]
    y <- m[, olcip]
    tplm <- MASS::rlm(y~x)
    tplm <- coef(tplm)
    tplm <- paste0("y = ", round(tplm[1],3), ifelse(tplm[2]>0, " + ", " - "), round(abs(tplm[2]), 3), "x")
    ymx  <- y - x
    mape <- mean(abs(ymx)/x, na.rm = TRUE) * 100
    mape <- paste0("MAPE (1:1): ", round(mape, 2), " %")
    bias <- mean((ymx)/x, na.rm = TRUE) * 100
    bias <- paste0("Bias (1:1): ", round(bias, 2), " %")
    rmse <- sqrt(mean((ymx)^2, na.rm = TRUE))
    rmse <- paste0("RMSE (1:1): ", round(rmse, 2), " ", xylab)
    rang <- range(na.omit(cbind(x, y))[, 1], na.rm = T)
    rang <- paste0("Range: ", round(rang[1], 2), " - ", round(rang[2], 2), " ", xylab)
    n    <- nrow(m)
    n    <- paste0("N: ", n)
    HTML(paste(tplm, mape, bias, rmse, n, rang, sep = '<br/>'))
  })

  output$olci_download_selection <- downloadHandler(
    filename = function() {
      paste("olci_matchups_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".csv", sep = "")
    },
    content = function(file) {
      write.csv(olci_matchups(), file, row.names = FALSE)
    }
  )

  output$olci_download_all <- downloadHandler(
    filename = function() {
      paste("olci_matchups_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".csv", sep = "")
    },
    content = function(file) {
      spatial <- input$olci_spt
      if(spatial == "Center pixel") {
        m_data <- m_data_p
      } else {
        m_data <- m_data_b
      }
      write.csv(m_data, file, row.names = FALSE)
    }
  )

}
