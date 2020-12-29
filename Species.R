library(data.table)
library(Rtsne)
library(ggplot2)
library(caret)
library(ggplot2)
library(ClusterR)
options(scipen = 999)
set.seed(3)

# load in data 
data<-fread("./project/volume/data/raw/data.csv")
data2 <- data.frame(lapply( data, factor)) 
dummies <- dummyVars(id~., data = data2)
Final<-predict(dummies, newdata = data2)

# we are not supposed to know the party of the individuals so we should hide this




# do a pca
pca<-prcomp(Final)

# look at the percent variance explained by each pca
screeplot(pca)

# look at the rotation of the variables on the PCs
pca

# see the values of the scree plot in a table 
summary(pca)

# see a biplot of the first 2 PCs
biplot(pca)

# use the unclass() function to get the data in PCA space
pca_dt<-data.table(unclass(pca)$x)
#pca_dt2<-data.table(unclass(pca)$rotation)




# see a plot with the party data 
ggplot(pca_dt,aes(x=PC1,y=PC2))+geom_point()



tsne<-Rtsne(pca_dt,pca = F,perplexity=40,check_duplicates = T,normalize=F,pca_center =F, pca_scale = F)
#tsne2<-Rtsne(Final,pca = T,perplexity=51,check_duplicates = F)


# grab out the coordinates
tsne_dt<-data.table(tsne$Y)
tsne<-Rtsne(pca_dt,pca = F,perplexity=51,check_duplicates = T,normalize=F,pca_center =F, pca_scale = F)
tsne_dt<-data.table(tsne$Y)
tsne<-Rtsne(pca_dt,pca = F,perplexity=20,check_duplicates = T,normalize=F,pca_center =F, pca_scale = F)
tsne_dt<-data.table(tsne$Y)

#tsne_dt2<-data.table(tsne2$Y)







# use a gaussian mixture model to find optimal k and then get probability of membership for each row to each group

# this fits a gmm to the data for all k=1 to k= max_clusters, we then look for a major change in likelihood between k values
k_bic<-Optimal_Clusters_GMM(tsne_dt[,.(V1,V2)],max_clusters = 10,criterion = "BIC")
#k_bic<-Optimal_Clusters_GMM(tsne_dt[,.(V1,V2)],max_clusters = 25,criterion = "BIC")

# now we will look at the change in model fit between successive k values
delta_k<-c(NA,k_bic[-1] - k_bic[-length(k_bic)])

# I'm going to make a plot so you can see the values, this part isnt necessary
del_k_tab<-data.table(delta_k=delta_k,k=1:length(delta_k))

# plot 
ggplot(del_k_tab,aes(x=k,y=-delta_k))+geom_point()+geom_line()+
  scale_x_continuous(breaks = scales::pretty_breaks(n = 10))+
  geom_text(aes(label=k),hjust=0, vjust=-1)




opt_k<-3

# now we run the model with our chosen k value
gmm_data<-GMM(tsne_dt[,.(V1,V2)],opt_k)

# the model gives a log-likelihood for each datapoint's membership to each cluster, me need to convert this 
# log-likelihood into a probability

l_clust<-gmm_data$Log_likelihood^1

l_clust<-data.table(l_clust)

net_lh<-apply(l_clust,1,FUN=function(x){sum(1/x)})

cluster_prob<-1/l_clust/net_lh




# we can now plot to see what cluster 1 looks like

C4<-tsne_dt$Cluster_1_prob<-cluster_prob$V1
C3<-tsne_dt$Cluster_2_prob<-cluster_prob$V2
C5<-tsne_dt$Cluster_3_prob<-cluster_prob$V3

CLUST3<-as.data.frame(C4)
ggplot(tsne_dt,aes(x=V1,y=V2,col=Cluster_1_prob))+geom_point()

sumbit<- data.table(id=data$id,species_1=tsne_dt$Cluster_1_prob,species_2=tsne_dt$Cluster_3_prob,species_3=tsne_dt$Cluster_2_prob)
fwrite(sumbit,"./project/volume/data/processed/submit_17.csv")
