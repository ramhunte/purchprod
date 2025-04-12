#' header
#'
#' @description A fct function
#'
#' @return The return value, if any, from executing the function.
#'
#' @noRd
header <- function() {
  # adding NOAA logo
  tags$head(
    tags$script(HTML(
      '$(document).ready(function() {
        $(".navbar .container-fluid")
          .css({"position": "relative"})
          .append("<img id=\'myImage\' src=\'www/noaa_header.png\'>");
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
