
# load data and librarys --------------------------------------------------

library(tidyverse)
library(magrittr)
library(mice)
library(caret)
library(doParallel)
logRMSE = function(data, lev = NULL, model = NULL){
  out = c(sqrt(mean((log(data[, "pred"], base = exp(1)) - log(data[, "obs"], base = exp(1)))^2)),
          sqrt(mean((data[, "pred"] - data[, "obs"])^2)))
  names(out) = c("logRMSE", "RMSE")
  out
}
# load data and remove "Id"
train = read_csv("C:/Users/User/Desktop/kaggle/house-prices-advanced-regression-techniques/train.csv") %>% dplyr::select(-Id)
test = read_csv("C:/Users/User/Desktop/kaggle/house-prices-advanced-regression-techniques/test.csv") %>% dplyr::select(-Id)
# data : (1460, 80)

colnames(train)

train$MSSubClass %<>% as.character()
test$MSSubClass %<>% as.character()

# plot(SalePrice~as.factor(MoSold), train)
# plot(SalePrice~as.factor(YrSold), train)
# plot(SalePrice~as.factor(YrMoSold), train %>% unite(YrMoSold, YrSold:MoSold) %>% select(YrMoSold, SalePrice))

train %<>% unite(YrMoSold, YrSold:MoSold, sep = "_")
test %<>% unite(YrMoSold, YrSold:MoSold, sep = "_")

train$YrMoSold %<>% as.factor() 
test$YrMoSold %<>% as.factor() 
identical(levels(train$YrMoSold),levels(test$YrMoSold))

colnames(train)[which(colnames(train) %in% c("1stFlrSF","2ndFlrSF","3SsnPorch"))] = c("FlrSF_1","FlrSF_2","snPorch_3S")
colnames(test)[which(colnames(test) %in% c("1stFlrSF","2ndFlrSF","3SsnPorch"))] = c("FlrSF_1","FlrSF_2","snPorch_3S")

# MSSubClass 
train$MSSubClass %>% table()
test$MSSubClass %>% table()
plot(SalePrice~MSSubClass, train)
# Alley & Street

train$Num_path = 2
test$Num_path = 2

train$Num_path[which(is.na(train$Alley))] = 1
test$Num_path[which(is.na(test$Alley))] = 1

train$Alley[which(is.na(train$Alley))] = "No Alley"
test$Alley[which(is.na(test$Alley))] = "No Alley"

train$Alley %>% table()
test$Alley %>% table()
train$Num_path %>% table()
test$Num_path %>% table()

train %<>% unite(Street_Alley, Street:Alley)
test %<>% unite(Street_Alley, Street:Alley)

train$Street_Alley %>% table()
test$Street_Alley %>% table()

# LotShape

plot(SalePrice~as.factor(LotShape), train)
plot(SalePrice~as.factor(LandContour), train)
plot(SalePrice~as.factor(LandSlope), train)

# time split

train %>% 
  dplyr::select(YrSold, MoSold, SalePrice) %>% 
  group_by(YrSold, MoSold) %>% 
  summarise(SalePrice = mean(SalePrice)) %>%
  ggplot()+
  geom_line(aes(x = as.numeric(MoSold), y = SalePrice, col = YrSold), size = 1.2)

train %>% group_by(YrSold, MoSold) %>% count(sort = T)
train %>% filter(YrSold == "2010") %>% filter(MoSold == "7") %>% dim()

# factor imbalance
train %>% group_by(MSSubClass) %>% count(sort = T)
train %>% group_by(MSZoning) %>% count(sort = T)
plot(SalePrice~as.factor(MSSubClass), train)
plot(SalePrice~as.factor(MSSubClass), train %>% filter(MSSubClass %in% c("150","40","45","180","75")))
lm(SalePrice~as.factor(MSSubClass), train) %>% anova()
lm(SalePrice~as.factor(MSSubClass), train %>% filter(MSSubClass %in% c("150","40","45","180","75"))) %>% anova()

train[which(train$MSSubClass %in% c("150","40")), "MSSubClass"] = "others"
test[which(test$MSSubClass %in% c("150","40")), "MSSubClass"] = "others"

# dealing with NA's -------------------------------------------------------

count_na = sapply(1:dim(train)[2], function(i){sum(is.na(train[,i]))})
# check NA columns
for(i in which(count_na != 0)){
  cat(colnames(train)[i], count_na[i], "\n")
}

count_na = sapply(1:dim(test)[2], function(i){sum(is.na(test[,i]))})
# check NA columns
for(i in which(count_na != 0)){
  cat(colnames(test)[i], count_na[i], "\n")
}

# MiscFeature PoolQC Fence Alley

train$Fence = plyr::mapvalues(train$Fence, from = c("GdPrv", "MnPrv", "GdWo", "MnWw", NA), to = c(100,90,80,70,0)) %>% as.numeric()
test$Fence = plyr::mapvalues(test$Fence, from = c("GdPrv", "MnPrv", "GdWo", "MnWw", NA), to = c(100,90,80,70,0)) %>% as.numeric()

train %<>% dplyr::select(-MiscFeature)
test %<>% dplyr::select(-MiscFeature)

# MasVnrType & MasVnrArea are really missing, and jointly missing
train %>% filter(MasVnrType == "None") %>% dplyr::select(MasVnrType, MasVnrArea) %>% table()
test %>% filter(MasVnrType == "None") %>% dplyr::select(MasVnrType, MasVnrArea) %>% table()

train %>% filter(!is.na(MasVnrType)) %>% filter(is.na(MasVnrArea)) %>% dplyr::select(MasVnrType, MasVnrArea)
train %>% filter(is.na(MasVnrType)) %>% filter(is.na(MasVnrArea)) %>% dplyr::select(MasVnrType, MasVnrArea)

test %>% filter(!is.na(MasVnrType)) %>% filter(is.na(MasVnrArea)) %>% dplyr::select(MasVnrType, MasVnrArea)
test %>% filter(is.na(MasVnrType)) %>% filter(!is.na(MasVnrArea)) %>% dplyr::select(MasVnrType, MasVnrArea)

# BsmtQual, BsmtCond, BsmtExposure, BsmtFinType1, BsmtFinType2 ------------

train %>% filter(is.na(BsmtQual), is.na(BsmtCond), is.na(BsmtExposure), is.na(BsmtFinType1), is.na(BsmtFinType2)) %>% dplyr::select(BsmtQual, BsmtCond, BsmtExposure, BsmtFinType1, BsmtFinType2) %>% is.na() %>% colSums()
test %>% filter(is.na(BsmtQual), is.na(BsmtCond), is.na(BsmtExposure), is.na(BsmtFinType1), is.na(BsmtFinType2)) %>% dplyr::select(BsmtQual, BsmtCond, BsmtExposure, BsmtFinType1, BsmtFinType2) %>% is.na() %>% colSums()

train$BsmtCond = plyr::mapvalues(train$BsmtCond, from = c("Ex", "Gd", "TA", "Fa", "Po"), to = c(100,90,80,70,60)) %>% as.numeric()
train$BsmtQual = plyr::mapvalues(train$BsmtQual, from = c("Ex", "Gd", "TA", "Fa", "Po"), to = c(100,90,80,70,60)) %>% as.numeric()

train$BsmtExposure = plyr::mapvalues(train$BsmtExposure, from = c("Gd", "Av", "Mn", "No"), to = c(90,80,70,60)) %>% as.numeric()
train$BsmtFinType1 = plyr::mapvalues(train$BsmtFinType1, from = c("GLQ", "ALQ", "BLQ", "Rec", "LwQ", "Unf"), to = c(100,90,80,70,60,50)) %>% as.numeric()
train$BsmtFinType2 = plyr::mapvalues(train$BsmtFinType2, from = c("GLQ", "ALQ", "BLQ", "Rec", "LwQ", "Unf"), to = c(100,90,80,70,60,50)) %>% as.numeric()

test$BsmtCond = plyr::mapvalues(test$BsmtCond, from = c("Ex", "Gd", "TA", "Fa", "Po"), to = c(100,90,80,70,60)) %>% as.numeric()
test$BsmtQual = plyr::mapvalues(test$BsmtQual, from = c("Ex", "Gd", "TA", "Fa", "Po"), to = c(100,90,80,70,60)) %>% as.numeric()

test$BsmtExposure = plyr::mapvalues(test$BsmtExposure, from = c("Gd", "Av", "Mn", "No"), to = c(90,80,70,60)) %>% as.numeric()
test$BsmtFinType1 = plyr::mapvalues(test$BsmtFinType1, from = c("GLQ", "ALQ", "BLQ", "Rec", "LwQ", "Unf"), to = c(100,90,80,70,60,50)) %>% as.numeric()
test$BsmtFinType2 = plyr::mapvalues(test$BsmtFinType2, from = c("GLQ", "ALQ", "BLQ", "Rec", "LwQ", "Unf"), to = c(100,90,80,70,60,50)) %>% as.numeric()

ind = which(is.na(train$BsmtQual)& is.na(train$BsmtCond)& is.na(train$BsmtExposure)& is.na(train$BsmtFinType1)&is.na(train$BsmtFinType2))
train[ind, c("BsmtQual", "BsmtCond", "BsmtExposure", "BsmtFinType1", "BsmtFinType2")] = 0
ind = which(is.na(test$BsmtQual)& is.na(test$BsmtCond)& is.na(test$BsmtExposure)& is.na(test$BsmtFinType1)&is.na(test$BsmtFinType2))
test[ind, c("BsmtQual", "BsmtCond", "BsmtExposure", "BsmtFinType1", "BsmtFinType2")] = 0

# GarageType, GarageYrBlt, GarageFinish, GarageQual, GarageCond  ----------

train %>% filter(is.na(GarageType), is.na(GarageYrBlt), is.na(GarageFinish), is.na(GarageQual), is.na(GarageCond)) %>% dplyr::select(GarageType, GarageYrBlt, GarageFinish, GarageQual, GarageCond) %>% is.na() %>% colSums()
test %>% filter(is.na(GarageType), is.na(GarageYrBlt), is.na(GarageFinish), is.na(GarageQual), is.na(GarageCond)) %>% dplyr::select(GarageType, GarageYrBlt, GarageFinish, GarageQual, GarageCond) %>% is.na() %>% colSums()

train$GarageQual = plyr::mapvalues(train$GarageQual, from = c("Ex", "Gd", "TA", "Fa", "Po"), to = c(100,90,80,70,60)) %>% as.numeric()
train$GarageCond = plyr::mapvalues(train$GarageCond, from = c("Ex", "Gd", "TA", "Fa", "Po"), to = c(100,90,80,70,60)) %>% as.numeric()

test$GarageQual = plyr::mapvalues(test$GarageQual, from = c("Ex", "Gd", "TA", "Fa", "Po"), to = c(100,90,80,70,60)) %>% as.numeric()
test$GarageCond = plyr::mapvalues(test$GarageCond, from = c("Ex", "Gd", "TA", "Fa", "Po"), to = c(100,90,80,70,60)) %>% as.numeric()


ind = which(is.na(train$GarageType) & is.na(train$GarageFinish) & is.na(train$GarageQual) & is.na(train$GarageCond))
train[ind, c("GarageType", "GarageFinish")] = "No Garage"
train[ind, c("GarageQual", "GarageCond")] = 0
train[ind, "GarageYrBlt"] = train[ind, "YearBuilt"]

ind = which(is.na(test$GarageType) & is.na(test$GarageFinish) & is.na(test$GarageQual) & is.na(test$GarageCond))
test[ind, c("GarageType", "GarageFinish")] = "No Garage"
test[ind, c("GarageQual", "GarageCond")] = 0
test[ind, "GarageYrBlt"] = test[ind, "YearBuilt"]

# count amount
count_na = sapply(1:dim(train)[2], function(i){sum(is.na(train[,i]))})
# check NA columns
for(i in which(count_na != 0)){
  cat(colnames(train)[i], count_na[i], "\n")
}

count_na = sapply(1:dim(test)[2], function(i){sum(is.na(test[,i]))})
# check NA columns
for(i in which(count_na != 0)){
  cat(colnames(test)[i], count_na[i], "\n")
}
# str(train[,c("Electrical", "BsmtFinType2", "BsmtExposure", "MasVnrArea", "MasVnrType", "LotFrontage")])
# Electrical factor level 5
# BsmtFinType2 factor level 6
# BsmtExposure factor level 5
# MasVnrArea num
# MasVnrType factor level 5
# LotFrontage num

train$HeatingQC %<>% plyr::mapvalues(., from = c("Ex", "Gd", "TA", "Fa", "Po"), to = c(100,90,80,70,60)) %>% as.numeric()
train$ExterCond %<>% plyr::mapvalues(., from = c("Ex", "Gd", "TA", "Fa", "Po"), to = c(100,90,80,70,60)) %>% as.numeric()
train$ExterQual %<>% plyr::mapvalues(., from = c("Ex", "Gd", "TA", "Fa", "Po"), to = c(100,90,80,70,60)) %>% as.numeric()
train$KitchenQual %<>% plyr::mapvalues(., from = c("Ex", "Gd", "TA", "Fa", "Po"), to = c(100,90,80,70,60)) %>% as.numeric()
train$FireplaceQu %<>% plyr::mapvalues(., from = c("Ex", "Gd", "TA", "Fa", "Po", NA), to = c(100,90,80,70,60,0)) %>% as.numeric()
train$PoolQC %<>% plyr::mapvalues(., from = c("Ex", "Gd", "TA", "Fa", NA), to = c(100,90,80,70,0)) %>% as.numeric()

test$HeatingQC %<>% plyr::mapvalues(., from = c("Ex", "Gd", "TA", "Fa", "Po"), to = c(100,90,80,70,60)) %>% as.numeric()
test$ExterCond %<>% plyr::mapvalues(., from = c("Ex", "Gd", "TA", "Fa", "Po"), to = c(100,90,80,70,60)) %>% as.numeric()
test$ExterQual %<>% plyr::mapvalues(., from = c("Ex", "Gd", "TA", "Fa", "Po"), to = c(100,90,80,70,60)) %>% as.numeric()
test$KitchenQual %<>% plyr::mapvalues(., from = c("Ex", "Gd", "TA", "Fa", "Po"), to = c(100,90,80,70,60)) %>% as.numeric()
test$FireplaceQu %<>% plyr::mapvalues(., from = c("Ex", "Gd", "TA", "Fa", "Po", NA), to = c(100,90,80,70,60,0)) %>% as.numeric()
test$PoolQC %<>%  plyr::mapvalues(., from = c("Ex", "Gd", "TA", "Fa", NA), to = c(100,90,80,70,0)) %>% as.numeric()
 
str(train)
train[is.na(train$FireplaceQu), "FireplaceQu"] = 0
test[is.na(test$FireplaceQu), "FireplaceQu"] = 0

# plot one variable -------------------------------------------------------

setwd("C:/Users/User/Desktop/kaggle/house-prices-advanced-regression-techniques/plots_one_var")
plot_bar = function(var_name){
  ind = which(colnames(train) == var_name)
  p = train %>% 
    group_by(is.na(LotFrontage)) %>% 
    count(!!! rlang::syms(names(.)[ind])) %>% 
    dplyr::mutate(ratio = n/sum(n), .keep = "all") %>%
    `colnames<-`(c("is_na", "var", "n", "ratio")) %>% 
    ggplot(., aes(x=var, y = ratio, fill=is_na)) +
    geom_bar(stat = 'identity', position = position_dodge2(width = .5, preserve = "single", padding = -0.5))+
    xlab(var_name)
  jpeg(paste0(var_name, "_bar.jpg"), width = 1080, height = 720)
  print(p)
  dev.off()
}
plot_density = function(var_name){
  ind = which(colnames(train) == var_name)
  p = train %>% 
    select(!!! rlang::syms(names(.)[ind]), LotFrontage) %>% 
    dplyr::mutate(
      is_na = ifelse(!is.na(LotFrontage),"FALSE","TRUE"), .keep = "all") %>%
    `colnames<-`(c("var", "LotFrontage", "is_na")) %>% 
    ggplot(., aes(x = var, color = is_na))+
    stat_density(geom = "line", position = "identity")+
    xlab(var_name)
  jpeg(paste0(var_name, "_density.jpg"), width = 1080, height = 720)
  print(p)
  dev.off()
}

# for(name in colnames(train[, sapply(train, class) != 'numeric'])){
#   plot_bar(name)
# }
# for(name in colnames(train[, sapply(train, class) == 'numeric'])){
#   if(name != "LotFrontage"){
#     plot_density(name)
#   }else{
#     print(name)
#   }
# }

# plot two variales -------------------------------------------------------

setwd("C:/Users/User/Desktop/kaggle/house-prices-advanced-regression-techniques/plots_two_var")
plot_box = function(var_name){
  ind = which(colnames(train) == var_name)
  p = train %>% 
    select(!!! rlang::syms(names(.)[ind]), SalePrice) %>% 
    `colnames<-`(c("var", "SalePrice")) %>% 
    ggplot(., aes(x = var, y = SalePrice))+
    geom_boxplot()+
    xlab(var_name)
  jpeg(paste0(var_name, "_box.jpg"), width = 1080, height = 720)
  print(p)
  dev.off()
}
plot_scatter = function(var_name){
  ind = which(colnames(train) == var_name)
  p = train %>% 
    select(!!! rlang::syms(names(.)[ind]), SalePrice) %>% 
    `colnames<-`(c("var", "SalePrice")) %>% 
    ggplot(., aes(x = var, y = SalePrice))+
    geom_point()+
    xlab(var_name)
  jpeg(paste0(var_name, "_scatter.jpg"), width = 1080, height = 720)
  print(p)
  dev.off()
}
# for(name in colnames(train[, sapply(train, class) != 'numeric'])){
#   plot_box(name)
# }
# for(name in colnames(train[, sapply(train, class) == 'numeric'])){
#   if(name != "SalePrice"){
#     plot_scatter(name)
#   }else{
#     print(name)
#   }
# }


# near zero variables -----------------------------------------------------

nzv = nearZeroVar(train, saveMetrics= TRUE)
nzv %>% arrange(desc(freqRatio)) %>% head(20)

table(train$Utilities) # one data, exist NA 
train %<>% dplyr::select(-Utilities) 
test %<>% dplyr::select(-Utilities) 

# high correlated variables -----------------------------------------------

high_cor_var = train %>% select(colnames(train[, sapply(train, class) == 'numeric'])) %>% dplyr::select(-SalePrice) %>% na.omit() %>% cor() %>% findCorrelation(., cutoff = 0.6, names = TRUE)
train %>% select(high_cor_var) %>% na.omit() %>% cor()
# feels good

# preprocess --------------------------------------------------------------

train_imp = mice(data = train, method = "cart", m = 5, maxit = 5) 
test_imp = mice(data = test, method = "cart", m = 5, maxit = 5) 
train_imp = complete(train_imp, action = 1)
test_imp = complete(test_imp, action = 1)

# count amount
count_na = sapply(1:dim(train_imp)[2], function(i){sum(is.na(train_imp[,i]))})
# check NA columns
for(i in which(count_na != 0)){
  cat(colnames(train_imp)[i], count_na[i], "\n")
}

count_na = sapply(1:dim(test_imp)[2], function(i){sum(is.na(test_imp[,i]))})
# check NA columns
for(i in which(count_na != 0)){
  cat(colnames(test_imp)[i], count_na[i], "\n")
}

table(train$MasVnrType) %>% sort()
table(train$Electrical) %>% sort()

train_imp[which(is.na(train_imp$MasVnrType)), "MasVnrType"] = "None"
train_imp[which(is.na(train_imp$Electrical)), "Electrical"] = "SBrkr"
anyNA(train_imp)

# tmp_train = train_imp %>% filter(!is.na(MasVnrType)) %>% dplyr::select(-c(BsmtExposure,BsmtFinType2,Electrical))
# tmp_test = train_imp %>% filter(is.na(MasVnrType)) %>% dplyr::select(-c(BsmtExposure,BsmtFinType2,Electrical))
# anyNA(tmp_train %>% dplyr::select(-MasVnrType))
# tree.fit = rpart(MasVnrType~, data = tmp_train)

table(test$MSZoning) 
table(test$Exterior1st) 
table(test$Exterior2nd) 
table(test$MasVnrType) 
table(test$Functional)  
table(test$GarageFinish) 
table(test$SaleType) 

train %>% filter(Exterior2nd == Exterior1st) %>% dim()
  
test_imp[which(is.na(test_imp$MSZoning)), "MSZoning"] = "RL"
test_imp[which(is.na(test_imp$Exterior1st)), "Exterior1st"] = "VinylSd"
test_imp[which(is.na(test_imp$Exterior2nd)), "Exterior2nd"] = "VinylSd"

test_imp[which(is.na(test_imp$MasVnrType)), "MasVnrType"] = "None"
test_imp[which(is.na(test_imp$Functional)), "Functional"] = "Typ"
test_imp[which(is.na(test_imp$GarageFinish)), "GarageFinish"] = "Unf"
test_imp[which(is.na(test_imp$SaleType)), "SaleType"] = "WD" 
anyNA(test_imp)

train_imp %>% str()

save.image("C:/Users/User/Desktop/kaggle/house-prices-advanced-regression-techniques/dataset.RData")
# model fitting  ----------------------------------------------------------
load("C:/Users/User/Desktop/kaggle/house-prices-advanced-regression-techniques/dataset.RData")


tcrl = trainControl(method = "cv", number = 5, summaryFunction = logRMSE, verboseIter = TRUE)
set.seed(1122)
# linear model with lasso & ridge mixed penalty ---------------------------

grid = expand.grid(alpha = seq(0.1,1,length.out = 10),
                   lambda = seq(0,1500,length.out = 50))

lm.fit = train(SalePrice ~ .,
               data = train_imp, 
               method = "glmnet", 
               trControl = tcrl,
               metric = "logRMSE",
               maximize = FALSE,
               tuneGrid = grid,
               verbose = FALSE)
lm.fit
plot(lm.fit,
     metric = "logRMSE", 
     xlab = "lambda",
     nameInStrip = TRUE,
     highlight = TRUE)

submit = read.csv("C:/Users/User/Desktop/kaggle/house-prices-advanced-regression-techniques/sample_submission.csv")
submit$SalePrice = predict(lm.fit, newdata = test_imp)
write.csv(submit, file = "C:/Users/User/Desktop/kaggle/house-prices-advanced-regression-techniques/lm_submit.csv", row.names = FALSE)

# gbm ---------------------------------------------------------------------

grid = expand.grid(shrinkage = seq(0.1,0.6,length.out = 6), 
                   interaction.depth= c(5,7,9), 
                   n.minobsinnode = c(6,8,10), 
                   n.trees = c(500,1000))
cl = makePSOCKcluster(5)
registerDoParallel(cl)
gbm.fit = train(SalePrice ~ .,
               data = train_imp, 
               method = "gbm", 
               trControl = tcrl,
               metric = "logRMSE",
               maximize = FALSE,
               tuneGrid = grid,
               verbose = FALSE)
stopCluster(cl)
gbm.fit
plot(gbm.fit,
     metric = "logRMSE", 
     nameInStrip = TRUE,
     highlight = TRUE)

submit = read.csv("C:/Users/User/Desktop/kaggle/house-prices-advanced-regression-techniques/sample_submission.csv")
submit$SalePrice = predict(gbm.fit, newdata = test_imp)
write.csv(submit, "C:/Users/User/Desktop/kaggle/house-prices-advanced-regression-techniques/gbm_submit.csv", row.names = FALSE)

# random forest -----------------------------------------------------------


grid = expand.grid(mtry = c(50,53,55,),
                   min.node.size = c(3,4,5,6,10),
                   splitrule = c("variance"))

cores = detectCores()
cl = makePSOCKcluster(cores-1)
registerDoParallel(cl)
rf.fit = train(SalePrice ~ .,
                data = train_imp, 
                method = "ranger", 
                trControl = tcrl,
                metric = "logRMSE",
                maximize = FALSE,
                tuneGrid = grid,
                verbose = FALSE)
stopCluster(cl)
rf.fit
plot(rf.fit,
     metric = "logRMSE", 
     nameInStrip = TRUE,
     highlight = TRUE)

submit = read.csv("C:/Users/User/Desktop/kaggle/house-prices-advanced-regression-techniques/sample_submission.csv")
submit$SalePrice = predict(rf.fit, newdata = test_imp)
write.csv(submit, "C:/Users/User/Desktop/kaggle/house-prices-advanced-regression-techniques/rf_submit.csv", row.names = FALSE)

