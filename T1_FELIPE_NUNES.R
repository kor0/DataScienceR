library(ggplot2)
##############
#LINK SECRETO#
# link <- https://www.passeidireto.com/arquivo/11095284/estatistica-basica---david-blackwell 
#############

#T1 Data Science HP - Felipe Boff Nunes
#Russian Presidential Elections 2018
#
#This dataset is available on Kaggle. It provides data about the 2018 Russian Presidential Election.
# link <- https://www.kaggle.com/valenzione/russian-presidental-elections-2018-voting-data/data#voting_data_eng.csv
data <- read.csv('./voting_data_eng.csv')
# Data Info:
# The dataset contains 94487 lines and 23 columns.
# It contains how many votes each candidate had and from where it came.
# Unfortunately, the dataset, although very well formated, 
# still was difficult to mess with as consequence of inexperience.


#for the for
candidates <- rep(NA, size)
# The summary function gives a good view of the dataset info.
summary(data)
     
attach(data)
#Multiplot of number of votes for candidate
#multiplot function in the end of the file#
p1 <- ggplot(data, aes(Baburin.Sergei.Nikolaevich)) + geom_histogram()
p2 <- ggplot(data, aes(Grudinin.Pavel.Nikolaevich)) + geom_histogram()
p3 <- ggplot(data, aes(Zhirinovskiy.Vladimir.Volfovich)) + geom_histogram()
p4 <- ggplot(data, aes(Putin.Vladimir.Vladimirovich)) + geom_histogram()
p5 <- ggplot(data, aes(Sobchak.Ksenia.Anatolyevna)) + geom_histogram()
p6 <- ggplot(data, aes(Suraikin.Maksim.Aleksandrovich)) + geom_histogram()
p7 <- ggplot(data, aes(Titov.Boris.Yurievich)) + geom_histogram()
p8 <- ggplot(data, aes(Yavlinskiy.Gregory.Alekseivich)) + geom_histogram()
multiplot(p1, p2, p3, p4, p5, p6, p7, p8, cols=4)
####

# Multiplot of number of votes for candidate by Region Name
th <- theme(axis.line=element_blank(),axis.text.x=element_blank(),
            axis.text.y=element_blank(),axis.ticks=element_blank(),
            axis.title.x=element_blank(),
            legend.position="none",
            panel.background=element_blank(),panel.border=element_blank(),panel.grid.major=element_blank(),
            panel.grid.minor=element_blank(),plot.background=element_blank())

q1 <- ggplot(data, aes(x=region_name, y=Baburin.Sergei.Nikolaevich))+
  geom_bar(width = 1, stat = "identity") + th 
q2 <- ggplot(data, aes(x=region_name, y=Grudinin.Pavel.Nikolaevich))+
  geom_bar(width = 1, stat = "identity") + th
q3 <- ggplot(data, aes(x=region_name, y=Zhirinovskiy.Vladimir.Volfovich))+
  geom_bar(width = 1, stat = "identity") + th
q4 <- ggplot(data, aes(x=region_name, y=Putin.Vladimir.Vladimirovich))+
  geom_bar(width = 1, stat = "identity") + th
q5 <- ggplot(data, aes(x=region_name, y=Sobchak.Ksenia.Anatolyevna))+
  geom_bar(width = 1, stat = "identity") + th
q6 <- ggplot(data, aes(x=region_name, y=Suraikin.Maksim.Aleksandrovich))+
  geom_bar(width = 1, stat = "identity") + th
q7 <- ggplot(data, aes(x=region_name, y=Titov.Boris.Yurievich))+
  geom_bar(width = 1, stat = "identity") + th
q8<- ggplot(data, aes(x=region_name, y=Yavlinskiy.Gregory.Alekseivich))+
  geom_bar(width = 1, stat = "identity") + th

multiplot(q1,q2,q3,q4,q5,q6,q7,q8, cols= 4)
####

# Here my intention was to see if there was some 
# correlation with the regions which voted less for Putin
# with the regions which had most of of the ballots lost (as ratio of N_votes to N_lost)
# Time and auto-diligence are my worst enemies, so I haven't finished it
r <- ggplot(data, aes(x=reorder(region_name, -Number.of.lost.ballot.papers), y=Number.of.lost.ballot.papers))+
  geom_bar(width = 1, stat = "identity")
r
####

#A função abaixo vem do
# link <- http://www.cookbook-r.com/Graphs/Multiple_graphs_on_one_page_(ggplot2)/
#
#
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
  require(grid)
  
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
