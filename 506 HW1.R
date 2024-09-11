##################
### Question 1 ###
##################
### (a) ###
data = read.table("C:/Users/z1883/Desktop/wine/wine.data", sep = ",", header = FALSE)
wine.data = data.frame(Type=data[,1],Alcohol=data[,2],Malic.acid=data[,3],
                       Ash=data[,4],Alcalinity.of.ash=data[,5],
                       Magnesium=data[,6],Total.phenols=data[,7],
                       Flavanoids=data[,8],Nonflavanoid.phenols=data[,9],
                       Proanthocyanins=data[,10],Color.intensity=data[,11],
                       Hue=data[,12],OD280.or.OD315=data[,13],
                       Proline=data[,14])
head(wine.data)
nrow(wine.data);ncol(wine.data)

### (b) ###     
table(wine.data[,1])
# From the result, we can find that Class 1 is 59, Class 2 is 71, Class 3 is 48,
# which is same as the wine.names reported.


### (c) ### 
# 1
cor(wine.data$Alcohol, wine.data$Color.intensity)

# 2
wine.class1 = wine.data[which(wine.data[,1] == 1),]
wine.class2 = wine.data[which(wine.data[,1] == 2),]
wine.class3 = wine.data[which(wine.data[,1] == 3),]
cor1 = cor(wine.class1$Alcohol, wine.class1$Color.intensity)
cor2 = cor(wine.class2$Alcohol, wine.class2$Color.intensity)
cor3 = cor(wine.class3$Alcohol, wine.class3$Color.intensity)
cor1;cor2;cor3

# 3
wine.data[which(wine.data[,11] == max(wine.data[,11])),2]

# 4
rownumber = nrow(wine.data[which(wine.data[,10]>wine.data[,4]),])
(rownumber/nrow(wine.data))*100


### (d) ### 
overall = apply(wine.data[,2:14],2,mean)
c1 = apply(wine.data[which(wine.data[,1] == 1),2:14],2,mean)
c2 = apply(wine.data[which(wine.data[,1] == 2),2:14],2,mean)
c3 = apply(wine.data[which(wine.data[,1] == 3),2:14],2,mean)
rbind(overall, c1, c2, c3)


### (e) ### 
class1 = wine.data[which(wine.data[,1] == 1),7]
class2 = wine.data[which(wine.data[,1] == 2),7]
class3 = wine.data[which(wine.data[,1] == 3),7]
n1 = length(class1)
n2 = length(class2)
n3 = length(class3)
mean1 = mean(class1)
mean2 = mean(class2)
mean3 = mean(class3)
var1 = var(class1)
var2 = var(class2)
var3 = var(class3)

# For class 1 and class 2
pooled.var12 = (((n1 - 1)*var1) + ((n2 - 1)*var2))/(n1 + n2 - 2)
t.stat12 = (mean1 - mean2)/sqrt(pooled.var12*(1/n1 + 1/n2))
pvalue12 = 2*pt(-abs(t.stat12), (n1+n2-2))
pvalue12

# For class 2 and class 3
pooled.var23 = (((n2 - 1)*var2) + ((n3 - 1)*var3))/(n2 + n3 - 2)
t.stat23 = (mean2 - mean3)/sqrt(pooled.var23*(1/n2 + 1/n3))
pvalue23 = 2*pt(-abs(t.stat23), (n2+n3-2))
pvalue23

# For class 1 and class 3
pooled.var13 = (((n1 - 1)*var1) + ((n3 - 1)*var3))/(n1 + n3 - 2)
t.stat13 = (mean1 - mean3)/sqrt(pooled.var13*(1/n1 + 1/n3))
pvalue13 = 2*pt(-abs(t.stat13), (n1+n3-2))
pvalue13

# We also need to adjust the p-value for comparison
pvalues = c(pvalue12, pvalue23, pvalue13)
adjust.pvalues = p.adjust(pvalues, method = "holm")
adjust.pvalues





##################
### Question 2 ###
##################
### (a)&(b) ###
data = read.csv("C:/Users/z1883/Desktop/AskAManager.csv",header = TRUE)
colnames(data) = c("No.","Timestamp","Age","Industry","Job Title",
                   "Job Title Supplement","Salary","Monetary Compensation",
                   "Currency of Salary","Other Currency",
                   "Additional Context of Income",
                   "Working Country","Working State(if in the US)",
                   "Working City","Overall Working Years",
                   "Working Years in your field",
                   "Highest education level","Gender","Race(indicate all)")

### (c) ###
us.data = data[which(data[,9] == "USD"),]
head(us.data)
# the number of rows before restrict
nrow(data)
# the number of rows after restrict
nrow(us.data)


### (d) ###
# There are three type of mistakes that we need to delete.
# 1, the age is under 18
# 2, the overall working years is smaller than working years in your field.
#    because it is nonsense, we need to delete the working years in your field is
#    larger the overall years.
# 3, since in age column is a range, so let the max age minus 18 is less than the min overall year, which means they start
#    work under 18, which is impossible, so, we also should delete these rows.

# Step 1, do the first mistake.
unique(us.data$Age)
us.data1 = us.data[-which(us.data[,3] == "under 18"),]


# Step 2, do the second mistake.
unique(us.data1$`Overall Working Years`)
unique(us.data1$`Working Years in your field`)
# From the result, we can find the categories of overall and your field are the same,
# so, we are easy to compare and we need to do some transformation to these two columns.
n1 = nrow(us.data1)
overall.label = c(1:n1)*0
for(i in 1:n1)
{
  if(us.data1[i,15] == "1 year or less"){overall.label[i] = 1}
  if(us.data1[i,15] == "2 - 4 years"){overall.label[i] = 2}
  if(us.data1[i,15] == "5-7 years"){overall.label[i] = 3}
  if(us.data1[i,15] == "8 - 10 years"){overall.label[i] = 4}
  if(us.data1[i,15] == "11 - 20 years"){overall.label[i] = 5}
  if(us.data1[i,15] == "21 - 30 years"){overall.label[i] = 6}
  if(us.data1[i,15] == "31 - 40 years"){overall.label[i] = 7}
  if(us.data1[i,15] == "41 years or more"){overall.label[i] = 8}
}
field.label = c(1:n1)*0
for(i in 1:n1)
{
  if(us.data1[i,16] == "1 year or less"){field.label[i] = 1}
  if(us.data1[i,16] == "2 - 4 years"){field.label[i] = 2}
  if(us.data1[i,16] == "5-7 years"){field.label[i] = 3}
  if(us.data1[i,16] == "8 - 10 years"){field.label[i] = 4}
  if(us.data1[i,16] == "11 - 20 years"){field.label[i] = 5}
  if(us.data1[i,16] == "21 - 30 years"){field.label[i] = 6}
  if(us.data1[i,16] == "31 - 40 years"){field.label[i] = 7}
  if(us.data1[i,16] == "41 years or more"){field.label[i] = 8}
}
us.data1 = cbind(us.data1, overall.label, field.label)
us.data2 = us.data1[-which(us.data1[,20]<us.data1[,21]),1:19]
us.data2
nrow(us.data2)


# Step 3, do the third mistake.
# 写注释的时候一定要写清，我们只需要比较一边即可
# 也就是最大的年龄减去18 还要小于最小的工作时间
# 则说明他是在18岁之前工作的，所以我们要除去
# 但是如果是年龄减去18 仍然比最大的工作年限还要大
# 这则没问题，说明他18之后就开始工作了。


unique(us.data2$Age)
unique(us.data2$`Overall Working Years`)

n2 = nrow(us.data2)
max.age = c(1:n2)*0
for(i in 1:n2)
{
  if(us.data2[i,3] == "18-24"){max.age[i] = 6}
  if(us.data2[i,3] == "25-34"){max.age[i] = 16}
  if(us.data2[i,3] == "35-44"){max.age[i] = 26}
  if(us.data2[i,3] == "45-54"){max.age[i] = 36}
  if(us.data2[i,3] == "55-64"){max.age[i] = 46}
  if(us.data2[i,3] == "65 or over"){max.age[i] = 66}
}
min.overall = c(1:n2)*0
for(i in 1:n2)
{
  if(us.data2[i,15] == "1 year or less"){min.overall[i] = 0}
  if(us.data2[i,15] == "2 - 4 years"){min.overall[i] = 2}
  if(us.data2[i,15] == "5-7 years"){min.overall[i] = 5}
  if(us.data2[i,15] == "8 - 10 years"){min.overall[i] = 8}
  if(us.data2[i,15] == "11 - 20 years"){min.overall[i] = 11}
  if(us.data2[i,15] == "21 - 30 years"){min.overall[i] = 21}
  if(us.data2[i,15] == "31 - 40 years"){min.overall[i] = 31}
  if(us.data2[i,15] == "41 years or more"){min.overall[i] = 41}
}

us.data2 = cbind(us.data2, max.age, min.overall)
us.data2
us.data3 = us.data2[-which(us.data2[,20]<us.data2[,21]),1:19]
nrow(us.data3)


### (e) ###
# First, let find whether they have NA value
sum(is.na(us.data3[,7]))
# From the result, we can find there is no NA value,
# So, we just need to eliminate the outliers of the salary
# In this section, we choose to use IQR Method to delete the outliers.
Q1 = quantile(us.data3[,7],0.25)
Q1
Q3 = quantile(us.data3[,7],0.75)
boxplot(us.data3[,7])
IQR = Q3 - Q1
IQR
low = Q1-1.5*IQR
up = Q3+1.5*IQR
low;up
# since low is negative, but salary cannot be negative
# So, we just need to consider the data which is larger than Q3+1.5*IQR

us.data4 = us.data3[-which(us.data3[,7]>up),]
us.data4
boxplot(us.data4[,7])
# from the new plot, we can find, this is much better than before.






##################
### Question 3 ###
##################
### (a) ###

#' check palindromic
#' @param positive integer 
#' @return a list which contain
#' whether it is a palindromic
#' and give us the reverse of the input
#' @examples if you input not a positive integer
#' it will stop run and give you a warning, if you input
#' 728827, it will return $isPalindromic TRUE, reversed 728827
isPalindromic = function(value)
{
  if(!is.numeric(value))
  {
    warning("Input must be a positive integer.")
    stop("This input is not a numeric, please try again.")
  }
  if(abs(value) != value)
  {
    warning("Input must be a positive integer.")
    stop("This input is not a positive number, please try again.")
  }
  if(value != as.integer(value))
  {
    warning("Input must be a positive integer.")
    stop("This input is not an integer, please try again.")
  }
  value = as.integer(value)
  input = as.character(value)
  rev.input = paste(rev(unlist(strsplit(input,NULL))),collapse="")
  rev.value = as.integer(rev.input)
  result <- (rev.value == value)
  list.data = list(result, rev.value)
  names(list.data) = c("isPalindromic","reversed")
  return(list.data)
}

isPalindromic(8)





### (b)&(c) ###
# 1. Splitting the number into two halves.
# 2. Mirroring the left half to form a candidate palindrome.
# 3. If the candidate is not greater than the original number, increment the left half and mirror again.





#' Find next palindromic
#' @param positive integer 
#' @return the next palindromic which is
#' strictly larger than the input.
#' @examples if you input not a positive integer
#' it will stop run and give you a warning
#' if you input 7668, the output is 7777.
nextPalindrome = function(value)
{
  next.palindromic = 0
  if(!is.numeric(value))
  {
    warning("Input must be a positive integer.")
    stop("This input is not a numeric, please try again.")
  }
  if(abs(value) != value)
  {
    warning("Input must be a positive integer.")
    stop("This input is not a positive number, please try again.")
  }
  if(value != as.integer(value))
  {
    warning("Input must be a positive integer.")
    stop("This input is not an integer, please try again.")
  }
  
  ##############################

  value = as.integer(value)
  if(value <=8 & value >=0 )
  {
    next.palindromic = value + 1
    return(next.palindromic)
  }
  if(log10(value) == as.integer(log10(value)))
  {
    next.palindromic = value + 1
    return(next.palindromic)
  }
  
  if(log10(value+1) == as.integer(log10(value+1)))
  {
    next.palindromic = value + 2
    return(next.palindromic)
  }

  
  input = as.character(value)
  n = nchar(input)
  if(n%%2 == 0)
  {
    leftinput = substr(input,1,n/2)
    rightinput = substr(input,((n/2)+1),n)
    rev.leftinput = paste(rev(unlist(strsplit(leftinput,NULL))),
                          collapse="")
    
    rev.leftvalue = as.integer(rev.leftinput)
    leftvalue = as.integer(leftinput)
    rightvalue = as.integer(rightinput)
    if(rev.leftvalue > rightvalue)
    {
      char = paste(leftinput,rev.leftinput,sep = "")
      next.palindromic = as.integer(char)
      return(next.palindromic)
    }
    if(rev.leftvalue <= rightvalue)
    {
      sign = TRUE
      while(sign)
      {
        leftvalue = leftvalue + 1
        leftinput = as.character(leftvalue)
        rev.leftinput = paste(rev(unlist(strsplit(leftinput,NULL))),
                              collapse="")
        char = paste(leftinput,rev.leftinput,sep = "")
        palindromic = as.integer(char)
        if(palindromic > value)
        {
          next.palindromic = palindromic
          sign = FALSE
          return(next.palindromic)
        }
      }
    }
  }else
  {
    leftinput = substr(input,1,(n-1)/2)
    left.middle.input = substr(input,1,(n+1)/2)
    rightinput = substr(input,(((n+1)/2)+1),n)
    rev.leftinput = paste(rev(unlist(strsplit(leftinput,NULL))),
                          collapse="")
    
    rev.leftvalue = as.integer(rev.leftinput)
    leftvalue = as.integer(leftinput)
    left.middle.value = as.integer(left.middle.input)
    rightvalue = as.integer(rightinput) 
    if(rev.leftvalue > rightvalue)
    {
      char = paste(left.middle.input,rev.leftinput,sep = "")
      next.palindromic = as.integer(char)
      return(next.palindromic)
    }
    if(rev.leftvalue <= rightvalue)
    {
      sign = TRUE
      while(sign)
      {
        left.middle.value = left.middle.value + 1
        left.middle.input = as.character(left.middle.value)
        num = nchar(left.middle.input)
        leftinput = substr(left.middle.input,1,(num-1))
        rev.leftinput = paste(rev(unlist(strsplit(leftinput,NULL))),
                              collapse="")
        char = paste(left.middle.input,rev.leftinput,sep = "")
        palindromic = as.integer(char)
        if(palindromic > value)
        {
          next.palindromic = palindromic
          sign = FALSE
          return(next.palindromic)
        }
      }
    }
  }
  
}
nextPalindrome(7668)
nextPalindrome(391)
nextPalindrome(9928)
nextPalindrome(19272719)
nextPalindrome(109)
nextPalindrome(2)
nextPalindrome(7269)
nextPalindrome(765431537)





















