
# load data and librarys --------------------------------------------------

library(tidyverse)
library(magrittr)
library(RANN)
library(caret)
# load data and remove "Id"
train = read_csv("C:/Users/User/Desktop/kaggle/house-prices-advanced-regression-techniques/train.csv") %>% dplyr::select(-Id)
train$SalePrice = log(train$SalePrice, base = exp(1))
test = read_csv("C:/Users/User/Desktop/kaggle/house-prices-advanced-regression-techniques/test.csv") %>% dplyr::select(-Id)
test$SalePrice = log(test$SalePrice, base = exp(1))
colnames(train)
dim(train)
# data : (1460, 80)
str(train)

plot(SalePrice~YearBuilt, data = train)
lm(SalePrice~YearBuilt, data = train) %>% summary()
plot(SalePrice~YearRemodAdd, data = train)
lm(SalePrice~YearRemodAdd, data = train) %>% summary()

train$MSSubClass %<>% as.character()
# train$YearBuilt %<>% as.character()
# train$YearRemodAdd %<>% as.character()
train$MoSold %<>% as.character()
train$YrSold %<>% as.character()
train$GarageYrBlt %<>% as.character()

test$MSSubClass %<>% as.character()
# test$YearBuilt %<>% as.character()
# test$YearRemodAdd %<>% as.character()
test$MoSold %<>% as.character()
test$YrSold %<>% as.character()
test$GarageYrBlt %<>% as.character()

colnames(train)[c(43,44,69)] = c("FlrSF_1","FlrSF_2","snPorch_3S")
colnames(test)[c(43,44,69)] = c("FlrSF_1","FlrSF_2","snPorch_3S")

# time split
plot(x = train$MoSold, y = train$SalePrice)
plot(x = train$YrSold, y = train$SalePrice)
train %>% 
  dplyr::select(YrSold, MoSold, SalePrice) %>% 
  group_by(YrSold, MoSold) %>% 
  summarise(SalePrice = mean(SalePrice)) %>%
  ggplot()+
  geom_line(aes(x = as.numeric(MoSold), y = SalePrice, col = YrSold), size = 1.2)

train %>% group_by(YrSold, MoSold) %>% count(sort = T)
train %>% filter(YrSold == "2010") %>% filter(MoSold == "7") %>% dim()

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
train$Alley[which(is.na(train$Alley))] = "No Alley"
train[is.na(train$Fence), "Fence"] = "No Fence"
train[is.na(train$PoolQC), "PoolQC"] = "No PoolQC"
train[is.na(train$MiscFeature), "MiscFeature"] = "No MiscFeature"
train[is.na(train$FireplaceQu), "FireplaceQu"] = "No Firespace"

test$Alley[which(is.na(test$Alley))] = "No Alley"
test[is.na(test$Fence), "Fence"] = "No Fence"
test[is.na(test$PoolQC), "PoolQC"] = "No PoolQC"
test[is.na(test$MiscFeature), "MiscFeature"] = "No MiscFeature"
test[is.na(test$FireplaceQu), "FireplaceQu"] ="No Fireplace"

# MasVnrType & MasVnrArea are really missing, and jointly missing
train %>% filter(MasVnrType == "None") %>% dplyr::select(MasVnrType, MasVnrArea) %>% table()
test %>% filter(MasVnrType == "None") %>% dplyr::select(MasVnrType, MasVnrArea) %>% table()

train %>% filter(!is.na(MasVnrType)) %>% filter(is.na(MasVnrArea)) %>% dplyr::select(MasVnrType, MasVnrArea)
train %>% filter(is.na(MasVnrType)) %>% filter(is.na(MasVnrArea)) %>% dplyr::select(MasVnrType, MasVnrArea)

test %>% filter(!is.na(MasVnrType)) %>% filter(is.na(MasVnrArea)) %>% dplyr::select(MasVnrType, MasVnrArea)
test %>% filter(is.na(MasVnrType)) %>% filter(!is.na(MasVnrArea)) %>% dplyr::select(MasVnrType, MasVnrArea)


# Some of BsmtQual, BsmtCond, BsmtExposure, BsmtFinType1, BsmtFinType2 are really missing
train %>% filter(is.na(BsmtQual), is.na(BsmtCond), is.na(BsmtExposure), is.na(BsmtFinType1), is.na(BsmtFinType2)) %>% dplyr::select(BsmtQual, BsmtCond, BsmtExposure, BsmtFinType1, BsmtFinType2) %>% is.na() %>% colSums()
ind = which(is.na(train$BsmtQual))
train[ind, c("BsmtQual", "BsmtCond", "BsmtExposure", "BsmtFinType1", "BsmtFinType2")] = "No Basement"

test %>% filter(is.na(BsmtCond), is.na(BsmtQual), is.na(BsmtExposure), is.na(BsmtFinType1),
                is.na(BsmtFinType2), is.na(BsmtUnfSF), is.na(BsmtFullBath), is.na(BsmtHalfBath)) %>%  select(BsmtQual, BsmtCond, BsmtExposure, BsmtFinType1, BsmtFinSF1, BsmtFinType2,BsmtFinSF2,BsmtUnfSF,TotalBsmtSF,BsmtFullBath,BsmtHalfBath)


test %>% filter(!is.na(BsmtCond)) %>% filter(is.na(BsmtQual)) %>%  select(BsmtQual, BsmtCond)
ind = which(is.na(test$BsmtCond) & is.na(test$BsmtQual))
test[ind, c("BsmtQual", "BsmtCond")] = "No Basement"



# GarageType, GarageYrBlt, GarageFinish, GarageQual, GarageCond 
train %>% filter(is.na(GarageType), is.na(GarageYrBlt), is.na(GarageFinish), is.na(GarageQual), is.na(GarageCond)) %>% dplyr::select(GarageType, GarageYrBlt, GarageFinish, GarageQual, GarageCond) %>% is.na() %>% colSums()
train  %>% dplyr::select(GarageType, GarageYrBlt, GarageFinish, GarageQual, GarageCond) %>% is.na() %>% colSums()

test %>% filter(is.na(GarageType), is.na(GarageYrBlt), is.na(GarageFinish), is.na(GarageQual), is.na(GarageCond)) %>% dplyr::select(GarageType, GarageYrBlt, GarageFinish, GarageQual, GarageCond) %>% is.na() %>% colSums()
test  %>% dplyr::select(GarageType, GarageYrBlt, GarageFinish, GarageQual, GarageCond) %>% is.na() %>% colSums()

train %>% filter(is.na(GarageType)) %>% dplyr::select(GarageType, GarageYrBlt, GarageFinish, GarageQual, GarageCond) %>% is.na() %>% colSums()
test %>% filter(is.na(GarageType)) %>% dplyr::select(GarageType, GarageYrBlt, GarageFinish, GarageQual, GarageCond) %>% is.na() %>% colSums()

ind = which(is.na(train$GarageType))
train[ind, c("GarageType", "GarageYrBlt", "GarageFinish", "GarageQual", "GarageCond")] = "No Garage"

ind = which(is.na(test$GarageType)& is.na(test$GarageYrBlt)& is.na(test$GarageFinish)& is.na(test$GarageQual)& is.na(test$GarageCond))
test[ind, c("GarageType", "GarageYrBlt", "GarageFinish", "GarageQual", "GarageCond")] = "No Garage"


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

for(name in colnames(train[, sapply(train, class) != 'numeric'])){
  plot_bar(name)
}
for(name in colnames(train[, sapply(train, class) == 'numeric'])){
  if(name != "LotFrontage"){
    plot_density(name)
  }else{
    print(name)
  }
}

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
for(name in colnames(train[, sapply(train, class) != 'numeric'])){
  plot_box(name)
}
for(name in colnames(train[, sapply(train, class) == 'numeric'])){
  if(name != "SalePrice"){
    plot_scatter(name)
  }else{
    print(name)
  }
}


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

train_knn = preProcess(as.data.frame(train), method = "knnImpute")
train_knn = predict(train_knn, newdata = train)

count_na = sapply(1:dim(train_knn)[2], function(i){sum(is.na(train_knn[,i]))})
# check NA columns
for(i in which(count_na != 0)){
  cat(colnames(train_knn)[i], count_na[i], "\n")
}


# model fitting  ----------------------------------------------------------

tcrl = trainControl(method = "cv", number = 10, verboseIter = FALSE)
set.seed(1122)
gbmFit1 = train(SalePrice ~ ., 
                data = train_knn, 
                method = "gbm", 
                trControl = tcrl,
                verbose = TRUE)
gbmFit1