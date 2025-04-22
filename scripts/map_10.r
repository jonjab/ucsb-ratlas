# map 10


# clean the environment and hidden objects
rm(list=ls())

# set map number
current_sheet <- 10
# set ggplot counter
current_ggplot <- 0

gg_labelmaker <- function(plot_num){
  gg_title <- c("Map:", current_sheet, " ggplot:", plot_num)
  plot_text <- paste(gg_title, collapse=" " )
  print(plot_text)
  current_ggplot <<- plot_num
  return(plot_text)
}

# every ggtitle() or labs() should be:
# ggtitle(gg_labelmaker(current_ggplot+1))
# end automagic ggtitle           #######