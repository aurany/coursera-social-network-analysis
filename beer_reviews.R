
#Import libraries
library(rgexf)

#
# IMPORT RAW DATA
#
beer.reviews <- data.frame()

# Select
skiprows <- 0
select <- 100000

system.time(

# Read chunks 
while(1){

  input <- scan("C://Users/Raz/Downloads/beeradvocate.txt", what=character(), n=(13*select), skip=skiprows, sep="\n", strip.white=TRUE)
 
  if (length(input) == 0){
    rm(input)
    break
  }
  
  input <- gsub('^([a-zA-Z]+)/([a-zA-Z]+):\\s', '', input)
  
  beerName    <- input[seq(1, (13*select), 13)]
  beerId      <- input[seq(2, (13*select), 13)]
  brewerId    <- input[seq(3, (13*select), 13)]
  beerStyle   <- input[seq(5, (13*select), 13)]
  beerGrade   <- as.numeric(input[seq(10, (13*select), 13)])
  profileName <- input[seq(12, (13*select), 13)]
  
  reviews <- data.frame(beerName, beerId, brewerId, beerStyle, beerGrade, profileName, stringsAsFactors=FALSE)
  beer.reviews <- rbind(beer.reviews, reviews)
    
  rm(input, reviews, beerName, beerId, brewerId, beerStyle, beerGrade, profileName)
  
  skiprows <- skiprows + (14 * select)
  
  print(paste('Processed:', skiprows, sep=' '))
  
}

)

rm(select, skiprows)

#
# FIND NETWORK
#

# Select top-rated
hist(beer.reviews$beerGrade)

beer.reviews.top <- beer.reviews[which(beer.reviews$beerGrade >= 5), ]

# Create new beers dataset with new ids
beerName <- sort(unique(beer.reviews.top$beerName))
beerId <- 1:length(beerName)

beers <- data.frame(beerId, beerName)

# Create beer review matrix and set to 0 intitially
review.count <- matrix(0, nrow=length(beers$beerName), ncol=length(beers$beerName))

rm(beerId, beerName)

profiles <- unique(beer.reviews.top$profileName)

iter <- 1

for (profile in profiles){
  rows <- subset(beer.reviews.top, profileName == profile)
  names <- unique(rows$beerName)
  ids <- beers[which(beers$beerName %in% names),]$beerId
  review.count[ids, ids] <- review.count[ids, ids] + 1
  
  if(iter/100 == round(iter/100)){
    print(paste("Processed: ", iter, sep=''))
  }
  iter <- iter + 1
  rm(rows, names, ids)
}

rm(iter, profile, profiles)


# Create dataframe
edges <- data.frame()

for(row in 1:nrow(review.count)){
    
  if (row == 1){
    next
  }

  selection <- review.count[row, 1:(row-1)]
  target <- which(selection>0)
  freq <- selection[target]
  source <- rep.int(row, length(target))
  
  if (length(target) > 0){
    output <- data.frame(source, target, freq)
    edges <- rbind(edges, output)
    rm(output)
  }
  
  if(row/100 == round(row/100)){
    print(paste("Processed: ", row, sep=''))
  }
  
}

rm(row, selection, source, target, freq)

#
# EXPORTING DATA
#

# First attempt
nodes <- beers
colnames(nodes) <- c('id','label')

edges.data <- edges[, 1:2]
edges.wgt <- edges[, 3]

nodes$label <- gsub("&", "and", nodes$label)
nodes$label <- gsub("\'", "", nodes$label)
nodes$label <- gsub("\"", "", nodes$label)

rm(review.count, beer.reviews, beer.reviews.top, beers, edges)

write.gexf(nodes, edges.data, edgesWeight=edges.wgt, output='C://temp/top_beers.gexf')

# Second attempt
edges <- edges.data
edges$wgt <- edges.wgt

edges.data2 <- subset(edges, wgt > 1)
nodes2 <- nodes[nodes$id %in% c(edges.data2$source, edges.data2$target), ]
edges.wgt2 <- edges.data2[, 3]
edges.data2 <- edges.data2[,1:2]

write.gexf(nodes2, edges.data2, edgesWeight=edges.wgt2, output='C://temp/top_beers.gexf')
