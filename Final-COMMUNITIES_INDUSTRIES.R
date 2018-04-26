#load libraries igraph,plyr,ggplot2 and plotly
library(igraph)
library(plyr)
library(ggplot2)
library(plotly)

#read data from csv
consumersupplier <- read.csv("C:\\Users\\ganeshl\\Downloads\\Project_Downloads\\sy_consup_sic4desc_fortboth_loc_sample.txt",sep="\t")
consumersupplier$X <- NULL

#get only the consumer and supplier descriptions from the data
con_sup_desc <- consumersupplier[,c("supplier_sic4_desc","consumer_sic4_desc")]

#replace ',' in the data with space
con_sup_desc$consumer_sic4_desc <- gsub(pattern = ",", replacement= " ", con_sup_desc$consumer_sic4_desc)
con_sup_desc$supplier_sic4_desc <- gsub(pattern = ",", replacement= " ", con_sup_desc$supplier_sic4_desc)

#replace space with hyphen
con_sup_desc$consumer_sic4_desc <- gsub(pattern = " ", replacement= "-", con_sup_desc$consumer_sic4_desc)
con_sup_desc$supplier_sic4_desc <- gsub(pattern = " ", replacement= "-", con_sup_desc$supplier_sic4_desc)

#convert descriptions to lower case
con_sup_desc$consumer_sic4_desc<-tolower(con_sup_desc$consumer_sic4_desc)
con_sup_desc$supplier_sic4_desc<-tolower(con_sup_desc$supplier_sic4_desc)

#change the column name to Source and Target
names(con_sup_desc)<- c("Source.Names","Target.Names")

#get the supplier and consumer in individual data frames to get the unique nodes
sup <- con_sup_desc[("Source.Names")]
con <- con_sup_desc[("Target.Names")]

#create common column name for rbind
names(sup)<-"name"
names(con)<-"name"
sup_con <- rbind(sup,con)

#get the unique nodes
sup_con <- unique(sup_con)

#create graph 
g <- graph.data.frame(d=con_sup_desc,directed=TRUE,vertices=sup_con)
summary(g)

#function to obtain the modularity, membership and communities using different community detection algorithms

set.seed(5) # set seed so that membership does not change

func_membership <- function(n, g)
{
  if(n==1)  #Walktrap community detection
{
set.seed(5) # set seed so that membership does not change
wc <<- cluster_walktrap(g)
mod <<-modularity(wc)   #modularity 
members <<- membership(wc) #tells which node falls in which community
comm <<-communities(wc)
} #each community and its members
  
else if(n==2)  #InfoMAP community detection
{  
set.seed(5) # set seed so that membership does not change
imc <<- cluster_infomap(g)
members <<- membership(imc)
comm <<- communities(imc)
mod <<- modularity(imc)
}

else if(n==3)  # edge betweenness community detection
{
set.seed(5) # set seed so that membership does not change
kc <<- cluster_edge_betweenness(g,bridges = TRUE)
members <<- membership(kc)
comm <<- communities(kc)
mod <<- modularity(kc)
}

else if(n==4) #leading_eigen community detection
{ 
set.seed(5) # set seed so that membership does not change
kf <<- cluster_leading_eigen(g)
members <<- membership(kf)
comm <<- communities(kf)
mod <<- modularity(kf)
}

else if(n==5) #label propagation community detection
{
set.seed(5) # set seed so that membership does not change
m <<- cluster_label_prop(g)
members <<- membership(m)
comm <<- communities(m)
mod <<- modularity(m)
}

else if(n==6)  #spin glass community detection
{
set.seed(5) # set seed so that membership does not change
s <<- cluster_spinglass(g)
members <<- membership(s)
comm <<-communities(s)
mod <<- modularity(s)
}}

func_membership(6,g) # just change the number for the required algorithm to run

# convert the members vector to a data frame
membership_df <- as.data.frame.vector(members)
#get the DESC column for merging
membership_df$Names <- rownames(membership_df)
#rename
names(membership_df)<-c("Membership","DESC")
#set rownames as NULL
rownames(membership_df) <- NULL
#generate ID cilumn of nodes for visualization purpose 
membership_df$ID <- seq(from=1, to=nrow(membership_df))
#reorder
membership_df<- membership_df[,c("ID","DESC","Membership")]
#merge to get Source IDS
con_sup_desc<- merge(con_sup_desc,membership_df,by.x="Source.Names",by.y = "DESC")
#set ID column as Source column
colnames(con_sup_desc)[3:4] <- c("Source","Source.Mem")
#merge to get Target IDS
con_sup_desc<- merge(con_sup_desc,membership_df,by.x="Target.Names",by.y="DESC")
#set ID column as Target column
colnames(con_sup_desc)[5:6]<-c("Target","Target.Mem")
#reorder
con_sup_desc <- con_sup_desc[,c("Source","Target","Source.Names","Target.Names","Source.Mem","Target.Mem")]


#get the graph data frame with 
func_graph <- function(con_sup_desc,membership_df)
{
g1 <<- graph.data.frame(d=con_sup_desc, directed = TRUE,vertices = membership_df)
}
func_graph(con_sup_desc,membership_df)

#Visualization of algorithm
#Part 1- Nodes
func_algo_viz_Nodes <- function(g1,membership_df)
{
  #get the node coordinates
  plotcord <<- data.frame(layout.fruchterman.reingold(g1))
  colnames(plotcord) = c("X1","X2")
  #get the node IDS corresponding to the node co-ordinates
  plotcord$ID <- rownames(plotcord)
  #get the membership for colors
  plotcord <<- merge(plotcord,membership_df,by.x="ID",by.y="ID")
}
func_algo_viz_Nodes(g1,membership_df)

#convert the numeric membership column into a categorical column
plotcord$Membership <- as.factor(plotcord$Membership)

#part-2 Edges
func_algo_viz_Edges <- function(g1,plotcord)
{
  #get edges, which are pairs of node IDs
  edgelist <<- get.edgelist(g1)
  #convert to a four column edge data frame with source and destination coordinates
  edges <- data.frame(plotcord[edgelist[,1],], plotcord[edgelist[,2],])
  colnames(edges)[2:3] <- c("X1","Y1")
  colnames(edges)[7:8] <- c("X2","Y2")
  rownames(edges) <- NULL
  assign("edges",edges,envir=.GlobalEnv)
}
func_algo_viz_Edges(g1,plotcord)

#Function to resize window
resize.win <- function(Width=n, Height=n)
{
  # works for windows
  dev.off(); # dev.new(width=6, height=6)
  windows(record=TRUE, width=Width, height=Height)
}

resize.win(15,10)
#use ggplot to plot the visualization
p <- ggplot() + geom_segment(aes(x=X1, y=Y1, xend = X2, yend = Y2), data=edges, size = 0.5, colour="grey") + geom_point(data=plotcord,aes(X1, X2,color=Membership,text=plotcord$DESC))
ggplotly(p)%>%layout(legend = list(x = 100, y = 0.5))

#Function to get a data of a particular membership from the above memberships obtained
#membership_df === nodes file for the graph of membership
func_mem_deep <- function(membership_df,n)
{
  #number of members in the commnunity
  no_of_members <<- dim(membership_df[membership_df$Membership==n, ])
  #members in the community 
  mem_n <<- data.frame(membership_df[which(membership_df$Membership==n),])   
}

func_mem_deep(membership_df,1)  #generates data frame containing members in the desired membership of interest

# output of above function is input of below:
#Get the table with all the maximal cliques in the membership
func_clique <- function(mem_n)
{
  mem_n_V1 <<- mem_n  #copy data frame for editing purpose
  mem_n_V1$Membership <<- NULL # make membership column NULL
  g2 <<- induced.subgraph(graph=g,vids = mem_n_V1$ID) # get induced sub graph with the IDs in the above data set
  cliques_mem_n <<- table(sapply(maximal.cliques(g2), length)) # get the no of cliques and the number of members in each clique
}
func_clique(mem_n)
#plot clique frequency
plot(cliques_mem_n, type = 'l', xlab = "Cliques", ylab = "Frequency",
     main = "Fully Connected Subgraph Frequency Of Membership 1", col = "red")


#choose the maximum clique. Here it is 14
func_max_clique <- function(g2,n)
{
clique_seg_mem_n <<- maximal.cliques(g2,n,n)
clique_seg_mem_n <<- lapply(clique_seg_mem_n,as_ids)
clique_seg_mem_n <<- lapply(clique_seg_mem_n,as.data.frame)
clique_seg_mem_n <<- lapply(clique_seg_mem_n,setNames,"name")
mem_n_V1$Color1 <<- as.numeric(mem_n_V1$DESC %in% clique_seg_mem_n[[1]]$name)
mem_n_V1$ID <<- NULL
rownames(mem_n_V1)<<-NULL
}
func_max_clique(g2,14)
write.csv(mem_n_V1,"C:\\Users\\ganeshl\\Downloads\\colmem1_Final.csv")

#assigning color=0 and 1 for nodes and edges. 1= if they belong to the clique; 0=they do not
#get Source and Target Names from Original file
con_sup_desc_mem_n <- con_sup_desc[,c("Source.Names","Target.Names")]
#create ID for the nodes extracted as part of membership
mem_n_V1$ID <- seq(from=1, to=(nrow(mem_n_V1)))
#reorder
mem_n_V1<-mem_n_V1[,c("ID","DESC","Color1")]
#get only the Source description that are part of the desired membership
edges_mem_n <- merge(con_sup_desc_mem_n,mem_n_V1,by.x="Source.Names",by.y="DESC")
#rename
colnames(edges_mem_n)[3:4]<-c("Source","Source.Color")
#get only the Target description that are part of the desired membership
edges_mem_n <- merge(edges_mem_n,mem_n_V1,by.x="Target.Names",by.y="DESC")
#rename
colnames(edges_mem_n)[5:6]<-c("Target","Target.Color")
#reorder
edges_mem_n <- edges_mem_n[,c("Source","Target","Source.Names","Target.Names","Source.Color","Target.Color")]

#get the graph data frame for desired membership 
func_graph <- function(edges_mem_n,mem_n_V1)
{
  g3 <<- graph.data.frame(d=edges_mem_n, directed = TRUE,vertices = mem_n_V1)
}
func_graph(edges_mem_n,mem_n_V1)

#Visualization of cliques
#Part 1- Nodes
func_algo_viz_Nodes_Clique <- function(g3,mem_n_V1)
{
  #get the node coordinates
  plotcord1 <<- data.frame(layout.fruchterman.reingold(g3))
  colnames(plotcord1) = c("X1","X2")
  #get the node IDS corresponding to the node co-ordinates
  plotcord1$ID <- rownames(plotcord1)
  #get the colors for the nodes
  plotcord1 <<- merge(plotcord1,mem_n_V1,by.x="ID",by.y="ID")
}
func_algo_viz_Nodes_Clique(g3,mem_n_V1)

#convert the numeric Color column into a categorical column
plotcord1$Color1 <- as.factor(plotcord1$Color1)

#part-2 Edges
func_algo_viz_Edges_Cliques <- function(g3,plotcord1)
{
  #get edges, which are pairs of node IDs
  edgelist1 <<- get.edgelist(g3)
  #convert to a four column edge data frame with source and destination coordinates
  edges1 <- data.frame(plotcord1[edgelist1[,1],], plotcord1[edgelist1[,2],])
  colnames(edges1)[2:3] <- c("X1","Y1")
  colnames(edges1)[7:8] <- c("X2","Y2")
  rownames(edges1) <- NULL
  assign("edges1",edges1,envir=.GlobalEnv)
}
func_algo_viz_Edges_Cliques(g3,plotcord1)

#Function to resize window
resize.win <- function(Width=n, Height=n)
{
  # works for windows
  dev.off(); # dev.new(width=6, height=6)
  windows(record=TRUE, width=Width, height=Height)
}

resize.win(15,10)
#use ggplot to plot the visualization
p <- ggplot() + geom_segment(aes(x=X1, y=Y1, xend = X2, yend = Y2), data=edges1, size = 0.5, colour="grey") + geom_point(data=plotcord1,aes(X1, X2,color=Color1,text=plotcord1$DESC))
ggplotly(p)%>%layout(legend = list(x = 100, y = 0.5))





