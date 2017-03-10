# Copy data out of R
copy.table <- function(obj, size = 4096) {
  clip <- paste('clipboard-', size, sep = '')
  f <- file(description = clip, open = 'w')
  write.table(obj, f, row.names = FALSE, sep = '\t')
  close(f)  
}


# Paste data into R
paste.table <- function() {
  f <- file(description = 'clipboard', open = 'r')
  df <- read.table(f, sep = '\t', header = TRUE)
  close(f)
  return(df)
}



# Multiple plot function
#
# ggplot objects can be passed in ..., or to plotlist (as a list of ggplot objects)
# - cols:   Number of columns in layout
# - layout: A matrix specifying the layout. If present, 'cols' is ignored.
#
# If the layout is something like matrix(c(1,2,3,3), nrow=2, byrow=TRUE),
# then plot 1 will go in the upper left, 2 will go in the upper right, and
# 3 will go all the way across the bottom.
#
multiplot <- function(..., plotlist=NULL, file, cols=1, layout=NULL) {
  library(grid)
  
  # Make a list from the ... arguments and plotlist
  plots <- c(list(...), plotlist)
  
  numPlots = length(plots)
  
  # If layout is NULL, then use 'cols' to determine layout
  if (is.null(layout)) {
    # Make the panel
    # ncol: Number of columns of plots
    # nrow: Number of rows needed, calculated from # of cols
    layout <- matrix(seq(1, cols * ceiling(numPlots/cols)),
                     ncol = cols, nrow = ceiling(numPlots/cols))
  }
  
  if (numPlots==1) {
    print(plots[[1]])
    
  } else {
    # Set up the page
    grid.newpage()
    pushViewport(viewport(layout = grid.layout(nrow(layout), ncol(layout))))
    
    # Make each plot, in the correct location
    for (i in 1:numPlots) {
      # Get the i,j matrix positions of the regions that contain this subplot
      matchidx <- as.data.frame(which(layout == i, arr.ind = TRUE))
      
      print(plots[[i]], vp = viewport(layout.pos.row = matchidx$row,
                                      layout.pos.col = matchidx$col))
    }
  }
}

# pull a column from tbl object into a vector
pull <- function(x,y) 
{
  x[,if(is.name(substitute(y))) deparse(substitute(y)) else y, drop = FALSE][[1]]
}

    
# convert sas DATE9. format to R
sas_to_r_date <- function(sas_date)
{
  r_date <- as.Date(sas_date, "%d%B%Y")
  return(r_date)
}
    
    
# convert date from mm/dd/yyyy to R date format
convert_date <- function(raw_date)
{
  raw_date <- as.Date(raw_date, format = '%m/%d/%Y')
  return(raw_date)
}
 
# put numbers in percnetage format
percent <- function(x, digits = 2, format = "f", ...) {
  paste0(formatC(100 * x, format = format, digits = digits, ...), "%")
  }
  
# Extract the year and the month of a date
yr_mnth <- function(date0)
{
  library(lubridate)
  
  year_n <- year(date0)
  mnth_n <- month(date0)
  
  paste(year_n, '-', str_pad(mnth_n, 2, pad = "0"), sep="")
}
    
    
# Extract the year and the week of a date
yr_wk <- function(date0)
{
  library(lubridate)
  
  year_n <- year(date0)
  week_n <- week(date0)
    
  paste(year_n, '-', str_pad(week_n, 2, pad = "0"), sep="")
}
