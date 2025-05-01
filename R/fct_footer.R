#' footer
#'
#' @description A fct function
#'
#' @return The return value, if any, from executing the function.
#'
#' @noRd

# this script makes a function that places a footer at the bottom of the app with links to external NOAA websites
footer <- function() {
  tags$footer(
    class = "footer text-center",
    style = "
      position: relative;
      bottom: 0;
      width: 100%;
      text-align: center;
      padding: 10px;
      font-size: 12px;
      background-color: #001743;
      color: #C2D9E3;",

    HTML(paste0(
      # NWFSC
      "<a href='https://www.fisheries.noaa.gov/region/west-coast/northwest-science' target='_blank' style='color: #C2D9E3;'>NWFSC</a> - ",
      # NOAA Fisheries
      "<a href='https://www.fisheries.noaa.gov/' target='_blank' style='color: #C2D9E3;'>NOAA Fisheries</a> - ",
      # NOAA
      "<a href='https://www.noaa.gov/' target='_blank' style='color: #C2D9E3;'>NOAA</a> - ",
      # NOAA privacy policy
      "<a href='https://www.fisheries.noaa.gov/about-us/privacy-policy' target='_blank' style='color: #C2D9E3;'>NOAA privacy policy </a> - ",
      format(Sys.Date(), "%Y")
    ))
  )
}
