clusterAndPlot = function(mat, distance, sales, type = "kMeans", k = 10){

if(type == "hclust"){
	hc <- hclust(dist(distance))
	z_ct = cutree(hc,k=k)
	clusterlabs = cbind(label = z_ct, row=1:dim(distance)[1])
	clusterlabs = clusterlabs[order(clusterlabs[,1]),]
	cluster = split(clusterlabs[,2], clusterlabs[,1])
	# plot sales for all the clusters
	PlotSales(cluster, sales)	
	cs = cluster.stats(distance,z_ct)
	return(list(clus = z_ct, cluster= cluster, cs = cs)) 
	}
###################################################################################################
if(type == "pam"){
	p = pam(dist(distance), k, diss = TRUE)
	pclusterlabs = cbind(label = p$clustering, row=1:dim(distance)[1])
	pclusterlabs = pclusterlabs[order(pclusterlabs[,1]),]
	pcluster = split(pclusterlabs[,2], pclusterlabs[,1])
	PlotSales(pcluster, sales)	
	cs = cluster.stats(distance,p$clustering)
	return(list(clus = p$clustering, cluster= pcluster, cs = cs)) 
	}
###################################################################################################
if(type == "kMeans"){
	M = apply(mat, 2,as.numeric) 
	km = kmeans((M), centers = k)
	kclusterlabs = cbind(label = km$cluster, row=1:dim(distance)[1])
	kclusterlabs = kclusterlabs[order(kclusterlabs[,1]),]
	kcluster = split(kclusterlabs[,2], kclusterlabs[,1]) 
	PlotSales(kcluster, sales)
	cs = cluster.stats(distance,km$cluster)
	return(list(clus = km, cluster= kcluster, cs = cs)) 
	}
}

PlotSales = function(cluster, sales){
	n_cluster=length(cluster)
	par(mfrow=c(ceiling(n_cluster/2),2),mar=c(2,2,2,2));
	for(cl in cluster){
	cl_data=t(sales[cl,])
	matplot(cl_data, type='l');
	# lines(1:dim(as.matrix(cl_data))[1],rowMeans(as.matrix(cl_data)),col = "red", lwd = 2)	
}
}