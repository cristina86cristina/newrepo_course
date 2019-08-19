setA <- list(manju=c("A","B","C"),georgie=c("B","C","E","F"),cristina=c("A","B","C","E"))
#setB <- list(c("B","C","E","F"))



gom.obj <- newGOM(setA)
check_m <- getMatrix(gom.obj, "Jaccard")
drawHeatmap(gom.obj, what="Jaccard")

check_m[lower.tri(check_m, diag = FALSE)] <- 1

#class(check_m) <- "numeric"
#dist(check_m,diag = TRUE)

drawHeatmap(check_m)
jaccard <- function(x,y){ #Specify how to calculate Jaccard Infex
  jInt <- intersect(x,y)
  jUnion <- unique(c(x,y))
  return(length(jInt)/length(jUnion))}



wrapper <- function(df) {
check <- list(df$members_Overlap)
#names(check) <- mydf2$name
}

mydf2 <- mydf %>% filter(name=="MyD88:Mal cascade initiated on plasma membrane" | name=="NFkB and MAP kinases activation mediated by TLR4 signaling repertoire")

check <-  mydf2  %>% group_by(name) %>%
  do(res = wrapper(.)) 


gom.obj <- newGOM(mydf2$members_Overlap)
getMatrix(gom.obj, "Jaccard")


#jInt <- intersect(setA,setB)
#jUnion <- unique(c(setA,setB))
#length(jInt)/length(jUnion)

make_list <- function(variable_genes){
  as.list(strsplit(variable_genes,", "))
}

mydf_d <- apply(mydf,1,make_list)

mylist <- rep(list(),length(mydf_d))

for(i in 1:length(mydf_d)) {
  
  mylist[[i]] <- (mydf_d[[i]]$members_Overlap)
}

names(mylist) <- mydf$name

jaccard <- function(x,y){ #Specify how to calculate Jaccard Infex
  jInt <- intersect(x,y)
  jUnion <- unique(c(x,y))
  return(length(jInt)/length(jUnion))}

outer(quarterback_names,draft_names,FUN=stringdist,method="jaccard",q=2)

#res <- matrix(nrow=length(mylist),ncol=length(mylist))
#for(i in 1:length(mylist)) {
 # res[i,1] <- jaccard()
 # mylist[[i]] <- (mydf_d[[i]]$members_Overlap)
#}
dist(check_m)
clusters <- hclust(dist(jaccard_overlap_m))
diag = FALSE, upper = FALSE,

pca_res <- prcomp(dist(result))
pca_res$x %>% 
  as.data.frame %>%
  ggplot(aes(x=PC1,y=PC2)) + geom_point(size=4) +
  theme_bw(base_size=32) 

library(Matrix)

outer(setA,FUN="jaccard")

