
# load data and librarys --------------------------------------------------

library(tidyverse)
library(magrittr)
library(mice)
library(caret)
# load data and remove "Id"
train = read_csv("C:/Users/User/Desktop/kaggle/house-prices-advanced-regression-techniques/train.csv") %>% dplyr::select(-Id)
test = read_csv("C:/Users/User/Desktop/kaggle/house-prices-advanced-regression-techniques/test.csv") %>% dplyr::select(-Id)
# data : (1460, 80)

plot(SalePrice~YearBuilt, data = train)
abline(lm(SalePrice~YearBuilt, data = train), col = "red")
lm(SalePrice~YearBuilt, data = train) %>% summary()
plot(SalePrice~YearRemodAdd, data = train)
abline(lm(SalePrice~YearRemodAdd, data = train), col = "red")
lm(SalePrice~YearRemodAdd, data = train) %>% summary()
plot(SalePrice~GarageYrBlt, data = train)
abline(lm(SalePrice~GarageYrBlt, data = train), col = "red")
lm(SalePrice~GarageYrBlt, data = train) %>% summary()
train %>% select(YearBuilt, GarageYrBlt, YearRemodAdd) %>% na.omit() %>% cor()

plot(SalePrice~as.factor(MoSold), train)
anova(lm(SalePrice~as.factor(MoSold), train))
plot(SalePrice~as.factor(YrSold), train)
anova(lm(SalePrice~as.factor(YrSold), train))

train$MSSubClass %<>% as.character()
# train$YearBuilt %<>% as.character()
# train$YearRemodAdd %<>% as.character()
# train$GarageYrBlt %<>% as.character()
train$MoSold %<>% as.character()
train$YrSold %<>% as.character()

test$MSSubClass %<>% as.character()
# test$YearBuilt %<>% as.character()
# test$YearRemodAdd %<>% as.character()
# test$GarageYrBlt %<>% as.character()
test$MoSold %<>% as.character()
test$YrSold %<>% as.character()

colnames(train)[c(43,44,69)] = c("FlrSF_1","FlrSF_2","snPorch_3S")
colnames(test)[c(43,44,69)] = c("FlrSF_1","FlrSF_2","snPorch_3S")

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
train$Alley[which(is.na(train$Alley))] = "No Alley"
train[is.na(train$Fence), "Fence"] = "No Fence"
train[is.na(train$PoolQC), "PoolQC"] = "No PoolQC"
train[is.na(train$MiscFeature), "MiscFeature"] = "No MiscFeature"
train[is.na(train$FireplaceQu), "FireplaceQu"] = "No Fireplace"

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

# BsmtQual, BsmtCond, BsmtExposure, BsmtFinType1, BsmtFinType2 ------------

train %>% filter(is.na(BsmtQual), is.na(BsmtCond), is.na(BsmtExposure), is.na(BsmtFinType1), is.na(BsmtFinType2)) %>% dplyr::select(BsmtQual, BsmtCond, BsmtExposure, BsmtFinType1, BsmtFinType2) %>% is.na() %>% colSums()
test %>% filter(is.na(BsmtQual), is.na(BsmtCond), is.na(BsmtExposure), is.na(BsmtFinType1), is.na(BsmtFinType2)) %>% dplyr::select(BsmtQual, BsmtCond, BsmtExposure, BsmtFinType1, BsmtFinType2) %>% is.na() %>% colSums()

ind = which(is.na(train$BsmtQual)& is.na(train$BsmtCond)& is.na(train$BsmtExposure)& is.na(train$BsmtFinType1)&is.na(train$BsmtFinType2))
train[ind, c("BsmtQual", "BsmtCond", "BsmtExposure", "BsmtFinType1", "BsmtFinType2")] = "No Basement"
ind = which(is.na(test$BsmtQual)& is.na(test$BsmtCond)& is.na(test$BsmtExposure)& is.na(test$BsmtFinType1)&is.na(test$BsmtFinType2))
test[ind, c("BsmtQual", "BsmtCond", "BsmtExposure", "BsmtFinType1", "BsmtFinType2")] = "No Basement"

# GarageType, GarageYrBlt, GarageFinish, GarageQual, GarageCond  ----------

train %>% filter(is.na(GarageType), is.na(GarageYrBlt), is.na(GarageFinish), is.na(GarageQual), is.na(GarageCond)) %>% dplyr::select(GarageType, GarageYrBlt, GarageFinish, GarageQual, GarageCond) %>% is.na() %>% colSums()
test %>% filter(is.na(GarageType), is.na(GarageYrBlt), is.na(GarageFinish), is.na(GarageQual), is.na(GarageCond)) %>% dplyr::select(GarageType, GarageYrBlt, GarageFinish, GarageQual, GarageCond) %>% is.na() %>% colSums()

ind = which(is.na(train$GarageType) & is.na(train$GarageFinish) & is.na(train$GarageQual) & is.na(train$GarageCond))
train[ind, c("GarageType", "GarageFinish", "GarageQual", "GarageCond")] = "No Garage"
train[ind, "GarageYrBlt"] = train[ind, "YearBuilt"]

ind = which(is.na(test$GarageType) & is.na(test$GarageFinish) & is.na(test$GarageQual) & is.na(test$GarageCond))
test[ind, c("GarageType", "GarageFinish", "GarageQual", "GarageCond")] = "No Garage"
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

# table(train$MasVnrType)
# table(train$BsmtExposure)
# table(train$BsmtFinType2) 
# table(train$Electrical)
train_imp[which(is.na(train_imp$MasVnrType)), "MasVnrType"] = "None"
train_imp[which(is.na(train_imp$BsmtExposure)), "BsmtExposure"] = "No"
train_imp[which(is.na(train_imp$BsmtFinType2)), "BsmtFinType2"] = "Unf"
train_imp[which(is.na(train_imp$Electrical)), "Electrical"] = "SBrkr"
anyNA(train_imp)

# tmp_train = train_imp %>% filter(!is.na(MasVnrType)) %>% dplyr::select(-c(BsmtExposure,BsmtFinType2,Electrical))
# tmp_test = train_imp %>% filter(is.na(MasVnrType)) %>% dplyr::select(-c(BsmtExposure,BsmtFinType2,Electrical))
# anyNA(tmp_train %>% dplyr::select(-MasVnrType))
# tree.fit = rpart(MasVnrType~, data = tmp_train)

table(test$MSZoning) %>% which.max()
table(test$Exterior1st) %>% which.max()
table(test$Exterior2nd) %>% which.max()
table(test$MasVnrType) %>% which.max()
table(test$BsmtQual) %>% which.max()
table(test$BsmtCond) %>% which.max()
table(test$BsmtExposure) %>% which.max()
table(test$KitchenQual) %>% which.max()
table(test$Functional) %>% which.max()
table(test$GarageYrBlt) %>% which.max()
table(test$GarageFinish)  %>% which.max()
table(test$GarageQual) %>% which.max()
table(test$GarageCond) %>% which.max()
table(test$SaleType) %>% which.max()

test_imp[which(is.na(test_imp$MSZoning)), "MSZoning"] = "RL"
test_imp[which(is.na(test_imp$Exterior1st)), "Exterior1st"] = "VinylSd"
test_imp[which(is.na(test_imp$Exterior2nd)), "Exterior2nd"] = "VinylSd"
test_imp[which(is.na(test_imp$MasVnrType)), "MasVnrType"] = "None"
test_imp[which(is.na(test_imp$BsmtQual)), "BsmtQual"] = "TA"
test_imp[which(is.na(test_imp$BsmtCond)), "BsmtCond"] = "TA"
test_imp[which(is.na(test_imp$BsmtExposure)), "BsmtExposure"] = "No"
test_imp[which(is.na(test_imp$KitchenQual)), "KitchenQual"] = "TA"
test_imp[which(is.na(test_imp$Functional)), "Functional"] = "Typ"
test_imp[which(is.na(test_imp$GarageYrBlt)), "GarageYrBlt"] = 2005
test_imp[which(is.na(test_imp$GarageFinish)), "GarageFinish"] = "Unf"
test_imp[which(is.na(test_imp$GarageQual)), "GarageQual"] = "TA"
test_imp[which(is.na(test_imp$GarageCond)), "GarageCond"] = "TA"
test_imp[which(is.na(test_imp$SaleType)), "SaleType"] = "WD" 
anyNA(test_imp)

save.image("C:/Users/User/Desktop/kaggle/house-prices-advanced-regression-techniques/dataset.RData")
# model fitting  ----------------------------------------------------------
load("C:/Users/User/Desktop/kaggle/house-prices-advanced-regression-techniques/dataset.RData")


tcrl = trainControl(method = "cv", number = 10, verboseIter = FALSE)
set.seed(1122)
# linear model with lasso & ridge mixed penalty ---------------------------

grid = expand.grid(alpha = seq(0.1,1,length.out = 10), lambda = seq(0,1500,length.out = 50))
lm.fit = train(SalePrice ~ .,
               data = train_imp, 
               method = "glmnet", 
               trControl = tcrl,
               metric = "RMSE",
               maximize = FALSE,
               tuneGrid = grid,
               verbose = FALSE)
lm.fit
plot(lm.fit,
     metric = "RMSE", 
     xlab = "lambda",
     nameInStrip = TRUE,
     highlight = TRUE)

submit = read.csv("C:/Users/User/Desktop/kaggle/house-prices-advanced-regression-techniques/sample_submission.csv")
submit$SalePrice = predict(lm.fit, newdata = test_imp)
write.csv(submit, file = "C:/Users/User/Desktop/kaggle/house-prices-advanced-regression-techniques/lm_submit.csv", row.names = FALSE)

# boosting ----------------------------------------------------------------

# grid = expand.grid(nrounds = 1000,
#                    max_depth = c(5,7,9),
#                    eta = ,
#                    gamma (Minimum Loss Reduction),
#                    subsample (Subsample Percentage),
#                    colsample_bytree (Subsample Ratio of Columns),
#                    rate_drop (Fraction of Trees Dropped),
#                    skip_drop = seq(0.4, 0.6),
#                    min_child_weight,)
xgb.fit = train(SalePrice ~ .,
                data = train_imp, 
                method = "xgbLinear", 
                trControl = tcrl,
                metric = "RMSE",
                objective = "reg:squarederror",
                maximize = FALSE,
                tuneLength = 10,
                verbose = TRUE)
xgb.fit
plot(xgb.fit,
     metric = "RMSE", 
     # xlab = "lambda",
     nameInStrip = TRUE,
     highlight = TRUE)

submit = read.csv("C:/Users/User/Desktop/kaggle/house-prices-advanced-regression-techniques/sample_submission.csv")
submit$SalePrice = predict(xgb.fit, newdata = test_imp)
write.csv(submit, file = "C:/Users/User/Desktop/kaggle/house-prices-advanced-regression-techniques/xgb_submit.csv", row.names = FALSE)

