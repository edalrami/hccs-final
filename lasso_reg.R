library(dplyr)
library(ggplot2)
library(glmnet)
X_train = read.csv("X_train.csv")
X_test = read.csv("X_test.csv")
y_train = read.csv("y_train.csv", header=FALSE)
y_test = read.csv("y_test.csv", header=FALSE)

X = rbind(X_train, X_test)
y = rbind(y_train, y_test)
#Check normality of target variable
qqnorm(y_train$V1, pch = 1, frame = FALSE)

#normality condition is violated. Transform the data
qqnorm(log(y_train$V1), pch = 1, frame = FALSE)
qqline(log(y_train$V1), col = "steelblue", lwd = 2)


#Fit the lasso model to the data
grid = 10^seq(10,-2,length=100)
lasso.mod = glmnet(as.matrix(X_train), as.matrix(log(y_train)), alpha=1, lambda = grid)

set.seed(1)
cv.out = cv.glmnet(as.matrix(X_train), as.matrix(log(y_train)), alpha=1 )
plot(cv.out)

#
bestlam = cv.out$lambda.min
lasso.pred = predict(lasso.mod, s = bestlam, newx = as.matrix(X_test))
mean((lasso.pred-y_test$V1)^2)

out = glmnet(as.matrix(X),as.matrix(y),alpha=1, lambda = grid)
lasso.coef = predict(out, type="coefficients", s=bestlam)
lasso.coef


features = as.data.frame(as.matrix(lasso.coef))
features

dim(features)

##Go through each row and determine if a value is zero
row_sub = apply(features, 1, function(row) all(row !=0 ))
##Subset as usual
features[row_sub,]

selected_features = rownames(as.data.frame(features))
selected_features
lasso.coef

#X[,c(selected_features[2:length(selected_features)])]
colnames(X)

#################################
#LOGISTIC REGRESSION
################################
X$pdc_80_flag

x_factors = model
#log_x = X[,-c("pdc_80_flag")]

#We already encoded the categorical variables in
#python. Therefore, we don't need to factorize the categoricals

#Retrieve the labels for all the categorical and continuous vars
cat_names = read.csv("cat_names.csv", header=TRUE)
cont_names = read.csv("cont_names.csv", header=TRUE)

categorical <- as.character(unlist(cat_names[1,]))
categorical

continuous <- as.character(unlist(cont_names[1,]))
continuous


getChi <- function(var, d){
  ##Return the chi-test hypothesis result
  #
  #Parameters: 
  #d: Dataframe containing cont and cat data
  #var: The column that we'll be doing chi-test on
  #
  #Returns: A list containing the var name and the pvalue
  t = d%>%
    select('pdc_80_flag', toString(var)) %>%
    table() %>%
    chisq.test()
  return(list(var , t$p.value))
}

d2 = read.csv("projectTrain.csv")
d2

chi_results = lapply(categorical, getChi, d2)
chi_results

chi_df = as.data.frame(matrix(unlist(chi_results), nrow = length(unlist(chi_results[1]))))
chi_df = t(chi_df)
colnames(chi_df) = c("var", "pval")
chi_df = as.data.frame(chi_df)

chi_df$var = as.character(chi_df$var)
chi_df$pval = as.numeric(as.character(chi_df$pval))

chi_df[(chi_df$pval < 0.05),]


###################################################################
#USE THESE FEATURES FOR KNN AND NAIVE BAYES AND LOGISTIC REGRESSION
###################################################################

#Selected features for logistic regression
selected_features2 = chi_df[(chi_df$pval < 0.05),]$var
selected_features2

selected_features2 = as.array(selected_features2) 

#selected_features2 = selected_features2[!selected_features2=='pdc_80_flag']

#### THESE ARE THE FEATURES TO USE FOR CLASSIFICATION
selected_features2



#Separate the categorical and cont vars
x_cats = d2[,which(colnames(d2) %in% selected_features2)]
x_cont = d2[,which(colnames(d2) %in% continuous)]
pdc_80_flag = d2$pdc_80_flag

#Factorize categoricals
x_factors = model.matrix(pdc_80_flag ~ ., data=x_cats)[,-1]
dim(x_factors)

continuous
#SELECT CONTINUOUS FEATURES
tt = t.test(d2$pre_er_cost ~ d2$pdc_80_flag, mu = 0, alternative = 'two.sided', conf = 0.95, var.eq = FALSE)


tt$p.value

library(car)
#Will need to check for levene test before doing t test
(t3 = d2%>%
    select(pdc_80_flag == 0))



#Get continuous features
t3 = d2[d2$pdc_80_flag == 0,]
t3[,c("pdc")]
t4 = shapiro.test(as.array(t3[,c("pdc")]))
t4$p.value
typeof(d2$pdc_80_flag)
t5 = leveneTest(d2$pdc ~ as.factor(d2$pdc_80_flag))
t5$`Pr(>F)`[1]

getCont <- function(var, data){
   flag1 = data[data$pdc_80_flag == 1,]
   flag0 = data[data$pdc_80_flag == 0,]
   
   #Check for equal variance when pdc_80_flag is 1 and 0
   n1 = shapiro.test(flag1[,c(var)])
   n2 = shapiro.test(flag0[,c(var)])
   
   if((n1.p.value > 0.05) & (n0.p.value > 0.05))
  
  
  
}





#x_ = as.matrix(data.frame(x_cont, x_factors))
#dim(x_)

#glmmod = glmnet(x_, y=as.factor(pdc_80_flag), alpha=1, family = "binomial")

#plot(glmmod, xvar = "lambda")

#glmmod

#coef(glmmod)[,10]

