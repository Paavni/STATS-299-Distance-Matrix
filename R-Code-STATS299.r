# install and load required packages
# install.packages(""); 
# library(); 
install.packages("R.utils")
install.packages("dtw");
install.packages("cluster");

library(R.utils)
library(dtw);
library(cluster);
library(fpc)

# read data
setwd("C:/Users/Paavni/My Academics/Quarter IV/STATS 299/Code")
sourceDirectory("Functions/")
data = read.csv("products.csv",header = TRUE, sep = ",")
sales = read.csv("sales.csv",header = TRUE, sep = ",")

data_feat= data[,-1]
data_feat$division = as.factor(data_feat$division)
data_feat$super_dept = as.factor(data_feat$super_dept)
data_feat$dept = as.factor(data_feat$dept)
data_feat$cat = as.factor(data_feat$cat)
data_feat$subcat = as.factor(data_feat$sub_cat)
data_feat$inv_type = as.factor(data_feat$inv_type)
# Generate feature space
print('generating features')
feat = data_feat[, c("division", "super_dept", "dept", "cat", "subcat", "inv_type", "curr_cost", "curr_retail", "ship_dim_x", "ship_dim_y", "ship_dim_z", "weight", "billable_weight")]
feat$div = factor(feat$division,labels = c(1:length(levels(feat$division))), ordered = is.ordered(feat$division))
feat$sup_dept = factor(feat$super_dept,labels = c(1:length(levels(feat$super_dept))), ordered = is.ordered(feat$super_dept))
feat$depa = factor(feat$dept,labels = c(1:length(levels(feat$dept))), ordered = is.ordered(feat$dept))
feat$cate = factor(feat$cat,labels = c(1:length(levels(feat$cat))), ordered = is.ordered(feat$cat))
feat$subcate = factor(feat$subcat,labels = c(1:length(levels(feat$subcat))), ordered = is.ordered(feat$subcat))
feat$in_ty = factor(feat$inv_type,labels = c(1:length(levels(feat$inv_type))), ordered = is.ordered(feat$inv_type))
feat_n = feat[, c("div", "sup_dept", "depa", "cate", "subcate", "in_ty", "curr_cost", "curr_retail", "ship_dim_x", "ship_dim_y", "ship_dim_z", "weight", "billable_weight")]
# compute shipping volume
shipping_volume = (feat_n$ship_dim_x*feat_n$ship_dim_y*feat_n$ship_dim_z)
shipping_volume_cat <- ( as.numeric( cut(shipping_volume,nclass.Sturges(shipping_volume))))
feat_n = dummy(feat_n, shipping_volume_cat, "vol")$dataframe
# change division and inventory type to binary
feat_n = dummy(feat_n, feat_n$div, "div")$dataframe
feat_n = dummy(feat_n, feat_n$in_ty, "inv_type")$dataframe
# Change to categorical feature space
feat_n <- (subset(feat_n, select = -c(1,6)))

# combine sales and other features
mat = cbind(sales, feat_n)
dat = cbind(sales, feat_n)
dat = dat[,-1]
mat = as.matrix(mat[,-1])

# separate different types of variables
ts = (mat[,1:365])
ts = apply(ts, 2,as.numeric)
w1 = dim(ts)[2]
qual = (mat[,366:369])
qual = apply(qual, 2,as.factor)
w2 = dim(qual)[2]
quant = (mat[,370:376])
quant = apply(quant, 2,as.numeric)
w3 = dim(quant)[2]
bin = (mat[,377:392])
bin = apply(bin, 2,as.factor)
w4 = dim(bin)[2]

#distance matrices for each variable
tsd = distance_ts(ts, cc=TRUE)
quald =distance_qual(qual) 
quantd = distance_quant(quant)
bind = distance_bin(bin)

tsd = as.matrix(tsd)
quald = as.matrix(quald)
quantd = as.matrix(quantd)
bind = as.matrix(bind)
#average
d_sum = (tsd + quald + quantd + bind)/4

tsn = normalization(ts)
qualn = normalization(apply(qual, 2,as.numeric))
quantn = normalization(quant)

#distance matrices for each variable
tsdn = distance_ts(tsn, cc= TRUE)
qualdn =distance_qual(qualn) 
quantdn = distance_quant(quantn)

tsdn = as.matrix(tsdn)
qualdn = as.matrix(qualdn)
quantdn = as.matrix(quantdn)
bind = as.matrix(bind)

#average
d_n_sum = (tsdn + qualdn + quantdn + bind)/4
#weighted average
d_n_sum_w = (w1*tsdn + w2*qualdn + w3*quantdn + w4*bind)/(w1+w2+w3+w4)
#distance matrix using daisy with standardization
d_n_daisy = as.matrix(daisy(dat, stand = TRUE))

################################################################################################################################################
# clustering on unnormalized data
k_cl = 10
#clustering
sales = apply(sales, 2,as.numeric)
#k-means
km1 = clusterAndPlot(mat, d_sum, sales, type = "kMeans", k = 10)
#pam
p1 = clusterAndPlot(mat, d_sum, sales, type = "pam", k = 10)
#hierarchical
hc1 = clusterAndPlot(mat, d_sum, sales, type = "hclust", k = 10)
################################################################################################################################################
# clustering on normalized data
k_cl = 10
#clustering
sales = apply(sales, 2,as.numeric)
#k-means
kmn1 = clusterAndPlot(mat, d_n_sum, sales, type = "kMeans", k = 10)
kmn2 = clusterAndPlot(mat, d_n_daisy, sales, type = "kMeans", k = 10)
kmn3 = clusterAndPlot(mat, d_n_sum_w, sales, type = "kMeans", k = 10)
#pam
pn1 = clusterAndPlot(mat, d_n_sum, sales, type = "pam", k = 10)
pn2 = clusterAndPlot(mat, d_n_daisy, sales, type = "pam", k = 10)
pn3 = clusterAndPlot(mat, d_n_sum_w, sales, type = "pam", k = 10)
#hierarchical
hcn1 = clusterAndPlot(mat, d_n_sum, sales, type = "hclust", k = 10)
hcn2 = clusterAndPlot(mat, d_n_daisy, sales, type = "hclust", k = 10)
hcn3 = clusterAndPlot(mat, d_n_sum_w, sales, type = "hclust", k = 10)
################################################################################################################################################


# plots
# cl_data=t(sales[kmn1$cluster[[1]],])
# matplot(cl_data, type='l', ylim =c(0,25), xlab = "Time", ylab = "Sales");
# cl_data=t(sales[hcn1$cluster[[5]][c(1:25)],])
# matplot(cl_data, type='l', ylim =c(0,25), xlab = "Time", ylab = "Sales");
	
