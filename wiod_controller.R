# initializations and libraries:
if (!require("igraph")) install.packages("igraph")
if(!require("Matrix")) install.packages("Matrix")
if(!require("lattice")) install.packages("lattice")
if (!require("vcd")) install.packages("vcd")
library(igraph)
library(lattice)
library(Matrix) 
library(vcd)
library(data.table)
# functions:
getWIOT <- function(period,
                    version) { # default and only option
                     
  ##
  ## sanity checks
  ##
  format <- 'wide'
  as.DT <- TRUE
  
  if((period < 2000 | period > 2014) & version == 'October16') {
    stop(" -> WIOTs are available for the years 2000 till 2014!")
  }
  
  if (period >= 2000 && period <= 2014 && version == 'October16') {
  
  if(!(format %in% c("wide", "long", "list"))) {
    stop(" -> The only possible format options are 'wide', 'long' or 'list'!")
  }
  
  if(version != "October16") {
    warning("No other version available. This option is without effect.")
  }
  
  if(!is.logical(as.DT)) {
    stop(" -> Please specify either TRUE or FALSE for the as.DT-option.")
  }
  
  if((as.DT == FALSE) & (format == "list")) {
    warning("For format = 'list', as.DT does not have an effect.")
  }
  
  ## WIOT2000_October16_ROW_list.rds
  base.url <- "http://wiiw.ac.at/files/staff-content/reiter/"
  
  res <- readRDS(file = gzcon(url(paste0(base.url, "WIOT", period, "_",
                                         version, "_ROW",
                                         ## "_", format,
                                         ifelse(format == "wide", "",
                                                paste0("_", format)),
                                         ".rds"))))
  
  ## load(file = url(paste0(base.url, "WIOT", period, "_",
  ##                        version, "_ROW",
  ##                        ifelse(format == "wide", "",
  ##                               paste0("_", format)),
  ##                        ".RData")))
  ## if(format == "wide") {
  ##     res <- wiot
  ## } else if(format == "long") {
  ##     res <- wiot.long
  ## } else if(format == "list") {
  ##     res <- wiot.list
  ## }
  if(format %in% c("wide", "long") & !as.DT) {
    ## print(format)
    ## print(as.DT)
    res <- as.data.frame(res)
  }
  # create the class :: iot
  # class(iot) <- list(year = res$Year, countries, industries, final, output, inter)
  # get the clusters:
  year <- as.numeric(period)
  industries <- unique(res$IndustryDescription)
  #final <- as.matrix()
  N <- length(industries) - 8
  industries <- industries[1:N]
  countries <- unique(res$Country) 
  G <- length(countries) - 1
  countries <- countries[1:G]
  year = unique(res$Year)
  # dimension of the inter matrix 
  GN <- G*N
  # create the final, 
  # output and inter matrices
  finIndex <- GN + 6
  res_rows <- dim(res)[1]
  res_cols = dim(res)[2]
  print(res_cols)
  final <- as.matrix(res[(1:GN),(finIndex:(res_cols - 1))], 
                     nrow = GN,
                     ncol = res_cols - GN - 6)
  output <- as.matrix(res[1:GN,res_cols:res_cols],nrow = GN, ncol = 1)
  inter<- as.matrix(res[1:GN,6:(GN + 5)], nrow = GN, ncol = GN)
  total <- as.matrix(res[(GN+1):(res_rows-1), (6:(GN + 5))], 
                     nrow = res_rows - GN - 1,
                     ncol = GN)
  valueAdded <- total[6]
  totalFinal <- as.matrix(res[(GN + 1):res_rows - 1, finIndex:(res_cols-1)],
                          nrow = res_rows - GN - 1,
                          ncol = res_cols - GN - 6)
  valueAddedFinal <- totalFinal[6]
  iot <- list(year = year,
              countries = countries,
              industries = industries,
              inter = inter,
              final = final, 
              total = total,
              valueAdded = valueAdded,
              output = output,
              totalFinal = totalFinal,
              valueAddedFinal = valueAddedFinal
              )
  class(iot) = "iot"
  return(iot)
  }
  if (version == 'October13') {
    if (period < 1995 | period > 2011) {
      stop("The wiod data from october 2013 database is available only for the years 1995 - 2011")
    }
    # set for every year specifical database ...
    if (period == 1995) {
      load("./wiod-2013/wiod_1995.RData")
    }
    if (period == 1996) {
      load("./wiod-2013/wiod_1996.RData")
    }
    if (period == 1997) {
      load("./wiod-2013/wiod_1997.RData")
    }
    if (period == 1998) {
      load("./wiod-2013/wiod_1998.RData")
    }
    if (period == 1999) {
      load("./wiod-2013/wiod_1999.RData")
    }
    if (period == 2000) {
      load("./wiod-2013/wiod_2000.RData")
    }
    if (period == 2001) {
      load("./wiod-2013/wiod_2001.RData")
    }
    if (period == 2002) {
      load('./wiod-2013/wiod_2002.RData')
    }
    if (period == 2003) {
      load('./wiod-2013/wiod_2003.RData')
    }
    if (period == 2004) {
      load("./wiod-2013/wiod_2004.RData")
    }
    if (period == 2005){
      load('./wiod-2013/wiod_2005.RData')
    }
    if (period == 2006) {
      load('./wiod-2013/wiod_2006.RData')
    }
    if (period == 2007) {
      load('./wiod-2013/wiod_2007.RData')
    }
    if (period == 2008) {
      load('./wiod-2013/wiod_2008.RData')
    }
    if (period == 2009) {
      load('./wiod-2013/wiod_2009.RData')
    }
    if (period == 2010) {
      load('./wiod-2013/wiod_2010.RData')
    }
    if (period == 2011) {
      load('./wiod-2013/wiod_2011.RData')
    }
    return(iot)
  }
}
a <- getWIOT(2000,"October16")
computeNTI <- function (countries,
                     year = 2000, 
                     dataset = 'October16',
                     industries = "all",
                     directed = FALSE) {
  # get the Input-Output table:
  io <- getWIOT(year,dataset)
  # set c(i,j) by 
  # the method that is shown in
  # ADB Economics Working Paper Series No. 263,
  # Mapping Vertical Trade,
  # Benno Ferrarin 
  # (see: http://citeseerx.ist.psu.edu/viewdoc/download?doi=10.1.1.876.9552&rep=rep1&type=pdf)
  G <- length(io$countries)
  N <- length(io$industries)
  if (!identical(countries %in% io$countries,c(T,T))) {
    stop('Incorrect countries declaration!')
  }
  if (industries = 'all') {
    # get the country indices:
    cInd <- match(countries, io$countries)
    ci <- cInd[1]
    cj = cInd[2]
    NTI <- 0
    for (i in 1:N) {
      rowcj <- (cj - 1)*N
      colci <- (ci - 1)*N + i
      csj <- sum(io$inter[,colci])
      sum_csi <- sum(io$inter[rowcj:rowcj + N - 1,colci])
      psi <- io$output[colci]
      sumpsi <- sum(io$output[(colci - i + 1):(colci - i + N)])
      NTI <- NTI + (csj/sum_csi)*(psi/sumpsi)
    }
    return(NTI)
  } else {
    # get the indices for every industry:
    if (is.character(industries)) {
      indices <- match(industries)
    } else indices <- industries
    for (i in indices){
      rowcj <- (cj - 1)*N
      colci <- (ci - 1)*N + i
      csj <- sum(io$inter[,colci])
      sum_csi <- sum(io$inter[rowcj:rowcj + N - 1,colci])
      psi <- io$output[colci]
      sumpsi <- sum(io$output[(colci - i + 1):(colci - i + N)])
      NTI <- NTI + (csj/sum_csi)*(psi/sumpsi)
    }
    return(NTI)
  }
}
computeAdjNTI <- function (year = 2000, 
                           dataset = "October16",
                           industries = "all",
                           symetric = F) {
  cnt <- getWIOT(year, dataset)
  countries <- cnt$countries
  G <- length(countries)
  N <- lenght(cnt$industries)
  C <- matrix(0,nrow = G, ncol = G)
  rownames(C) <- countries
  colnames(C) <- countries
  for (country in countries) {
    for (countryj in countries) {
      if (country != countryj) {
        C[country,countryj] = computeNTI(c(country, countryj),
                                         year,
                                         database,
                                         industries)
      }
    }
  }
  # get the max and min index of the matrix
  cmin <- min(C)
  cmax <- max(C)
  # normalize the matrix C
  for (i in countries) {
    for (j in countries) {
      C[i,j] <- (C[i,j] - cmin)/(cmax - cmin)
    }
  }
  # the matrix C is now 
  # the adjacency matrix 
  # of the network of GVC ...
  if (symetric) {
    for (i in countries) {
      for (j in countries) {
        C[i,j] <- 0.5*(C[i,j] + C[j,i])
        C[j,i] <- C[i,j]
      }
    }
  }
  return (C)
}
# ploting of the network:
# in following analysis I
# have used fragments from
# the source code of 
# G.Balas 
# construct the graph:
AdjM <- computeAdjNTI() # complete other args!!!
myGraph <- graph_from_adjacency_matrix(adjM,mode = "directed",
                                       weighted = TRUE,
                                       diag = FALSE,
                                       add.colnames = TRUE) %>%
  set_edge_attr("weight", value = AdjM) %>%
myGraph
node_size <- c()
cnum <- nrow(AdjM)
for (i in 1:cnum) {
  node_size <- c(node_size,sum(AdjM[1:cnum,i:i]))
}
node_size <- 10*node_size

V(myGraph)$name <- countries
plot(myGraph, layout = layout.fruchterman.reingold, vertex.size = 
       node_size, vertex.label.dist = .8, vertex.label.degree = 
       -pi/2, edge.width = E(myGraph)$weight)

# mutual links: In this section
# I used the code that is maden
# from G.Balas
mutgS <- which_mutual(myGraph, es = E(myGraph))
gswM <- myGraph-edges(E(myGraph)[mutgS])
gsM <- simplify(as.undirected(myGraph, mode = "mutual"), 
                remove.multiple = TRUE)
plot(gsM, layout = layout.fruchterman.reingold, vertex.size = 
       node_size, vertex.label.dist = .8, vertex.label.degree = 
       -pi/2, edge.arrow.size = .2)
## Degree centrality

outdcgF <- degree(myGraph, mode = "out", loops = F, 
                  normalized = F)
indcgF <- degree(myGraph, mode = "in", loops = F, 
                 normalized = F)
summary(indcgF)
summary(outdcgF)
outdcgF <- degree(myGraph, mode = "out", loops = F, 
                  normalized = T)
indcgF <- degree(myGraph, mode = "in", loops = F, 
                 normalized = T)
rescale <- function (nchar, low, high) {
  min_d <- min(nchar)
  max_d <- max(nchar)
  rscl <- ((high-low)*(nchar-min_d))/(max_d-min_d)+low
  rscl
}

sizindc <- rescale(indcgF, 1, 15)
sizoutdc <- rescale(outdcgF, 1, 15)

par(mfrow = c(1, 2))
plot(myGraph, layout = layout.fruchterman.reingold, vertex.size = 
       sizindc, vertex.label.dist = .8, vertex.label.degree = 
       -pi/2, edge.arrow.size = .1)
plot(myGraph, layout = layout.fruchterman.reingold, vertex.size = 
       sizoutdc, vertex.label.dist = .8, vertex.label.degree = 
       -pi/2, edge.arrow.size = .1)
par(mfrow = c(1, 1))

## Hubs-Authorities

hbgs <- hub_score(myGraph, scale = T , weights = NULL)
augs <- authority_score(myGraph, scale = T, weights = NULL)
sizhb <- rescale(hbgs$vector, 1, 15)
sizau <- rescale(augs$vector, 1, 15)
par(mfrow = c(1, 2))
plot(myGraph, layout = layout.fruchterman.reingold, vertex.size = 
       sizhb, vertex.label.dist = .8, vertex.label.degree = 
       -pi/2, edge.arrow.size = .1)
plot(myGraph, layout = layout.fruchterman.reingold, vertex.size = 
       sizau, vertex.label.dist = .8, vertex.label.degree = 
       -pi/2, edge.arrow.size = .1)
par(mfrow = c(1, 1))

# Page-Rank 
prgs <- page_rank(myGraph, algo = "arpack", directed = T,
                  damping = 0.85, personalized = NULL,
                  weights = NULL)
sizpr <- rescale(prgs$vector, 1, 15)

plot(myGraph, layout = layout.fruchterman.reingold, vertex.size = 
       sizpr, vertex.label.dist = .8, vertex.label.degree = 
       -pi/2, edge.arrow.size = .1)
# sensitivity analysis of the network: 
cutvgstr<-articulation_points(myGraph)
V(myGraph)$color<-"lightblue1"
V(myGraph)[as_ids(cutvgstr)]$color<-'yellow'
num_comp <- components(myGraph)$no
mstgstr<-minimum.spanning.tree(myGraph)
br<-c()

k<-0
for (i in 1:length(E(mstgstr))) {
  comp <- delete.edges(myGraph, as_ids(E(mstgstr)[i]))
  if ( components(comp)$no > num_comp ) {
    k<-k+1
    br[k]=as_ids(E(mstgstr)[i])
  }
}
br

E(myGraph)$color<-"grey"
E(myGraph)[as_ids(E(myGraph))=="ROW|IND"]$color<-"red"

plot(myGraph, layout = layout.fruchterman.reingold, vertex.size = 
       siztr, vertex.label.dist = .8, vertex.label.degree = 
       -pi/2, edge.arrow.size = .1)

V(myGraph)$color <- "orange"
E(myGraph)$color <- "darkgrey"

# Clustering-Communities Walktrap
wc <- cluster_walktrap(myGraph)
plot(wc, myGraph, layout = layout.fruchterman.reingold,
     vertex.label.dist = 1.5, vertex.label.degree = 
       -pi/2, edge.arrow.size = .1)
