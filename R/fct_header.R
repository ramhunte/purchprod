#' header
#'
#' @description A fct function
#'
#' @return The return value, if any, from executing the function.
#'
#' @noRd

# this script makes a function that places the NOAA logo in the navbar top right hand corner

header <- function() {
  # adding NOAA logo
  tags$head(
    tags$script(HTML(
      '$(document).ready(function() {
        $(".navbar .container-fluid")
          .css({"position": "relative"})
          .append("<a href=\'https://www.fisheries.noaa.gov\' target=\'_blank\'><img id=\'myImage\' src=\'www/noaa_header.png\'></a>");
      });'
    )),
    tags$style(HTML(
      '
      .navbar {
        min-height: 56px !important;
      }

      #myImage {
        position: absolute;
        right: 10px;
        top: -10px;
        height: 75px;
      }

      @media (max-width: 992px) {
        #myImage {
          position: fixed;
          right: 10%;
          top: 0.5%;
        }
      }
      '
    ))
  )
}
