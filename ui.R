library(ggvis)

# For dropdown menu
actionLink <- function(inputId, ...) {
  tags$a(href='javascript:void',
         id=inputId,
         class='action-button',
         ...)
}

fluidPage(
  HTML(
    paste("<h2><center><b>Interactive Evaluation of Remote Sensing retrievals",
    "over the Belgian Coastal Zone</b></center></h2>")
  ),
  tabsetPanel(
    tabPanel("OLCI",
      fluidRow(
        column(4,
          wellPanel(
            h4(tags$b("Filters")),
            selectInput(inputId = "olci_prd", 
              label = "OLCI product", c("Chl a (OC4Me)", "Chl a (NN)", 
              "SPM (NN)", "Turbidity", "PAR", "AOT(865)", "Rrs(400)", "Rrs(412)", 
              "Rrs(442)", "Rrs(490)", "Rrs(510)", "Rrs(560)", "Rrs(620)", 
              "Rrs(665)", "Rrs(674)", "Rrs(681)", "Rrs(709)", "Rrs(754)", 
              "Rrs(779)", "Rrs(865)", "Rrs(885)", "Rrs(1020)"), 
              selected = "Rrs(665)"
            ),
            selectInput(inputId = "olci_dtst", 
              label = "In situ dataset", c("All", "LifeWatch BE", "BMDC", 
              "AERONET-OC", "CEFAS SmartBuoys"), selected = "All"
            ),
            selectInput(inputId = "olci_spt", 
              label = "Spatial sampling", c("Center pixel", 
              "Box (3x3, excluding center pixel)"), selected = "Center pixel"
            ),
            sliderInput(inputId = "olci_maxt", 
              label = "Maximum time difference in matchup (hours)", min = 0.5, 
              max = 12, value = 3, step = 0.5
            ),
            sliderInput(inputId = "olci_vza", 
              label = "View zenith angle range (°)", min = 0, max = 60, 
              value = c(0, 60), step = 5
            ),
            sliderInput(inputId = "olci_sza", 
              label = "Sun zenith angle range (°)", min = 0, max = 80, 
              value = c(0, 70), step = 5
            ),
            sliderInput(inputId = "olci_aot", 
              label = "Aerosol Optical Thickness range at 865 nm (unitless)", 
              min = 0, max = 1, value = c(0, 1), step = 0.1),
            dateRangeInput(inputId = 'olci_trng', 
              label = 'Date range: (yyyy-mm-dd)', start = as.Date("2017-07-01"), 
              end = as.Date("2019-11-30"), min = as.Date("2017-07-01"), 
              max = as.Date("2019-11-30")
            )
          )
        ),
        column(4,
          HTML(
            paste("<center><h4><i>Hover over the points for details.</i>",  
            "</h4></center>")
          ),
          ggvisOutput("plot1"),
          wellPanel(
            h4(tags$b("Statistics")),
            textOutput("olci_mape"),
            textOutput("n_data"),
            htmlOutput("olci_stats")
          )
        ),
        column(4,
          wellPanel(
            HTML("Download data subset (.csv):<br><br>"),
            downloadButton(outputId = "olci_download_selection", label = "Download selection"),
            downloadButton(outputId = "olci_download_all", label = "Download all"),
          ),
          wellPanel(
            h3(tags$b("Information")),
            tags$p("Matchup dataset updated monthly. Latest update: 2019-12-04."),
            tags$p("When multiple observations for a given position and day", 
              "exist (e.g., AERONET-OC), only the closest to the overpass time", 
              "is shown. The complete dataset contains all matchups within the", 
              "maximum time difference of 12 hours."),
            HTML(
              paste("<p>Additional OLCI processing to remove unmasked clouds and",
              "large sun glint made based on a threshold of R<sub>rs</sub>(1020)",
              "> 0.005 sr. Note however that cloud shadows are apparent in OLCI",
              "distributed products and are not removed in the current version.</p>")
            ),
            HTML(
              paste("<p>AERONET-OC water leaving radiance converted to ",
              "hemispherical-directional water volume reflectance (R<sub>rs", 
              "</sub>) with the downwelling irradiance model of <a href =", 
              "https://doi.org/10.4319/lo.1990.35.8.1657>Gregg and Carder", 
              "(1990)</a> assuming maritime aerosols. No bandshifting is", 
              "performed in the current version; AERONET-OC bands assigned to", 
              "the closest matching OLCI band.</p>")
            ),
            h4(tags$b("Datasets")),
            HTML(
              paste("<p>Sentinel-3 OLCI Level 2 data provided by Copernicus",
              "and available on the <a href = https://coda.eumetsat.int/#/home>", 
              "Copernicus Online Data Access (CODA)</a> and on the <a href =",
              "https://archive.eumetsat.int/usc/>EUMETSAT Data Centre</a>.")
            ),
            HTML(
              paste("Details on the atmospheric correction, quality flags and",
              "algorithms for the Level 2 products are provided on the",
              "<a href = ", paste0("https://www.eumetsat.int/website/wcm/idc/",
              "idcplg?IdcService=GET_FILE&dDocName=PDF_DMT_907205&Revision",
              "SelectionMethod=LatestReleased&Rendition=Web"), ">Sentinel-3", 
              "OLCI Marine User Handbook</a>.")
            ),
            HTML(
              paste("General information on the Sentinel-3 OLCI sensors is",
              "provided on the",
              "<a href =https://sentinels.copernicus.eu/web/sentinel/user-guides/sentinel-3-olci>Sentinel-3 OLCI User Guide</a>.")
            ),
            HTML(
              paste("<p>In situ data from the following sources: </p>",
              "<ul>",
              "<li><a href = http://rshiny.lifewatch.be/station-data/>LifeWatch BE</a></li>",
              "<li><a href = http://www.bmdc.be/NODC/index.xhtml>BMDC</a></li>", 
              "<li><a href = https://aeronet.gsfc.nasa.gov/new_web/ocean_color.html>AERONET-OC</a> (Level 1.5)</li>",
              "<li><a href = http://wavenet.cefas.co.uk/Smartbuoy/Map>CEFAS SmartBuoys</a></li>",
              "</ul>")
            )
          )
        )
      )
    ),
    tabPanel("MODIS", h2("Not available at the moment")),
    tabPanel("VIIRS", h2("Not available at the moment")),
    tabPanel("OLI", h2("Not available at the moment")),
    tabPanel("MSI", h2("Not available at the moment")),
    tabPanel("About the online tool",
      column(4, offset = 4, 
        h2(tags$b("Information about this online tool")),
        HTML(
          paste("<p>This tool was conceived as a demonstration product of",
          "interactive validation analysis for remote sensing products. It is", 
          "intended both for specialists working with product development and", 
          "users at research and government institutions that intend to evaluate",
          "if data is suitable for their application.</p>")
        ),
        HTML(
          paste("<p>Data exploration is made easy with a web-server application",
          "(<a href = https://shiny.rstudio.com>R Shiny</a>) and the full range", 
          "of data is available for download and specialized analysis over", 
          "subsets (months, season, processing code, algorithm, etc.). Data is",
          "processed monthly on a dedicated computer and the table of results",
          "is uploaded to the interactive exploration tool. The goal is to have",
          "an inclusive program to add different processing codes, algorithms",
          "and sensors.</p>")
        ),
        HTML(
          "<p>The source code for this application is available <a href = https://github.com/AlexCast/rival_be>here</a>.</p>"
        ),
        h4(tags$b("Contact")),
        tags$p(tags$a(href="mailto:alexandre.castagna@ugent.be", "Alexandre Castagna"))
      )
    )
  )
)


