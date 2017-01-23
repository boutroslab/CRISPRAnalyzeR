#' Google theme for highcharts
#' 
#' Google theme for highcharts based on https://books.google.com/ngrams/
#' 
#' @param ... Named argument to modify the theme
#' 
#' @examples 
#' 
#' hc_demo() %>% 
#'   hc_add_theme(hc_theme_google2())
#' 
#' @export
hc_theme_google2 <- function(...){
  
  theme <-
    list(
      colors =  c("#F44336", "#2196F3", "#4CAF50", "#FFEB3B","#9C27B0","#03A9F4","#8BC34A","#FF9800","#3F51B5","#E91E63","#CDDC39","#FF5722"),
      chart = list(
        style = list(
          fontFamily = "Roboto",
          color = '#212121'
        )
      ),
      xAxis = list(
        gridLineWidth = 1,
        gridLineColor = '#F3F3F3',
        lineColor = '#F3F3F3',
        minorGridLineColor = '#F3F3F3',
        tickColor = '#F3F3F3',
        tickWidth = 1
      ),
      yAxis = list(
        gridLineColor = '#F3F3F3',
        lineColor = '#F3F3F3',
        minorGridLineColor = '#F3F3F3',
        tickColor = '#F3F3F3',
        tickWidth = 1
      ),
      plotOptions = list(
        boxplot = list(
          fillColor = "#FAFAFA"
        )
      ),
      
      legendBackgroundColor = 'rgba(0, 0, 0, 0.5)',
      background2 = '#505053',
      dataLabelsColor = '#B0B0B3',
      textColor = '#C0C0C0',
      contrastTextColor = '#F0F0F3',
      maskColor = 'rgba(255,255,255,0.3)'
    )
  
  theme <- structure(theme, class = "hc_theme")
  
  if (length(list(...)) > 0) {
    theme <- hc_theme_merge(
      theme,
      hc_theme(...)
    )
  } 
  
  theme
  
}