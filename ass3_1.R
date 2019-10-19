version

#import R library
if(!require("xlsx")) install.packages("xlsx",dependencies=TRUE)
library(xlsx)
if(!require("igraph")) install.packages("igraph",dependencies=TRUE)
library(igraph)
#read node
Nodes <- read.xlsx2(file="F:\\UTS\\FEIT_Lecture\\Semester_2\\Data Visualization and Visual Analytics\\ass_3\\ass3.xlsx",sheetIndex=1)
print(Nodes)

#data cleaning
Nodes$ID <- as.character(Nodes$ID)
Nodes[13,1] <- c("12")
Nodes[14,1] <- c("13")
Nodes[15,1] <- c("14")
Nodes[16,1] <- c("15")
Nodes[17,1] <- c("16")
Nodes[18,1] <- c("17")

#read edge
Edges <- read.xlsx2(file="F:\\UTS\\FEIT_Lecture\\Semester_2\\Data Visualization and Visual Analytics\\ass_3\\ass3.xlsx",sheetIndex=2)
str(Edges)
#data cleaning
Edges$ID <- as.character(Edges$ID)
Edges$ID.1 <- as.character(Edges$ID.1)
Edges$X.Emails_per_month <- as.integer (Edges$X.Emails_per_month)
Edges$X.Weight <- as.integer (Edges$X.Weight)

TempData<-data.frame(ID=c("8","13","14","0"),X.Emails_per_month=c(0,0,0,0),X.Weight=c(1,1,1,1),ID.1=c("8","13","14","0"))

Edges <- rbind(Edges,TempData)
names(Edges) <- c("From","count","weight","To")
Edges <- cbind(Edges[1],Edges[4],Edges[3])
#generate graph data

# undirected graph
g<-graph_from_data_frame(d= Edges, vertices= Nodes,directed=FALSE)
#weight attribute
graph_attr(g,'weigh')<- Edges$weight
#data cleaning
E(g)
g<-delete_edges(g,E(g)[49])
g<-delete_edges(g,E(g)[48])
g<-delete_edges(g,E(g)[47])
g<-delete_edges(g,E(g)[46])

#visualize
#plot(g)
#plot(g, vertex.shape="none",edge.label=graph_attr(g,'weigh'),vertex.label=V(g)$Name....)
#plot(g, edge.label=graph_attr(g,'weigh'))

#different weight and different line size and color
E(g)$weight

colorList<-c("red","green","blue","gray","cyan","pink","orange")
for(i in 1: length(E(g)))
{
  E(g)[c(i)]$width=E(g)$weight[i]
  #E(g)[c(i)]$width=3
  E(g)[c(i)]$color<- colorList[E(g)$weight[i]]
}
#V(g)$color<-c("white")
plot(g, layout= layout.fruchterman.reingold ,vertex.label=V(g)$Name....)

legend(x=-1.8,y=-0.5,c("Weight 1","Weight 2","Weight 3","Weight 4","Weight 5","Weight 6","Weight 7"),pch=21,col="#777777",pt.bg= colorList,pt.cex=2,cex=.8,bty="n",ncol=1)

#clustering
coms<-spinglass.community(g)

plot(coms,g,layout=layout.fruchterman.reingold,vertex.size=10,vertex.label.cex=.8,edge.arrow.size=.15)

#coms <- cluster_fast_greedy(g)
#coms <- multilevel.community(g, weights=NA)
#plot(coms,g,layout=layout.fruchterman.reingold,vertex.size=10,vertex.label.cex=.8,edge.arrow.size=.15)

V(g)
#plot(g,layout=layout_with_lgl,mark.groups=list(c(1,2,3),c(4,5,6) ,c(7,8,9) ,c(10,11,12) ,c(13,14,15) ,c(16,17,18)),mark.col=c("#C5E5E7","#ECD89A","#A1D891","#B21812","#C3D85A","#D418BA"), mark.border=NA)
#plot(g,layout=layout.fruchterman.reingold,mark.groups=list(c(1,2,3),c(4,5,6) ,c(7,8,9) ,c(10,11,12) ,c(13,14,15) ,c(16,17,18)),mark.col=c("#C5E5E7","#ECD89A","#A1D891","#B21812","#C3D85A","#D418BA"),mark.border=NA)
plot(g,layout=layout_in_circle,mark.groups=list(c(1,2,3),c(4,5,6) ,c(7,8,9) ,c(10,11,12) ,c(13,14,15) ,c(16,17,18)),mark.col=c("#C5E5E7","#ECD89A","#A1D891","#B21812","#C3D85A","#D418BA"),mark.border=NA)
plot(g,layout=layout_on_grid,mark.groups=list(c(1,2,3),c(4,5,6) ,c(7,8,9) ,c(10,11,12) ,c(13,14,15) ,c(16,17,18)),mark.col=c("#C5E5E7","#ECD89A","#A1D891","#B21812","#C3D85A","#D418BA"),mark.border=NA)