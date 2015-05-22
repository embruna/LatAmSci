####################################
####### 0: loading packages ########
####################################

require(ggplot2)
require(mgcv)


####################################
###### 1: Introducing the GAM ######
####################################

######### 1.1 #############
set.seed(10)
n = 250
x = runif(n,0,5)
y_model = 3*x/(1+2*x)
y_obs = rnorm(n,y_model,0.1)
data_plot = qplot(x, y_obs)+geom_line(aes(y=y_model))+theme_bw()
print(data_plot)


######### 1.2 #############
linear_model = gam(y_obs~x)
model_summary=summary(linear_model)
print(model_summary)
data_plot = data_plot+geom_line(colour="red", aes(y=fitted(linear_model)))
print(data_plot)



######### 1.3 #############
gam_model = gam(y_obs~s(x))
summary(gam_model)
data_plot = data_plot + 
  geom_line(colour="blue",aes(y=fitted(gam_model)))
print(data_plot)


######### 1.4 #############
plot(gam_model)


######### 1.5 #############
linear_model = gam(y_obs~x)
nested_gam_model = gam(y_obs~s(x)+x)
print(anova(linear_model, nested_gam_model, test="Chisq"))

######### 1.6 #############
n= 250
x_test = runif(n,-5,5)
y_test_fit = 4*dnorm(x_test)
y_test_obs = rnorm(n,y_test_fit, 0.2)


####################################
##### 2: Multiple smooth terms #####
####################################


######### 2.1 #############
gam_data = gamSim(eg=5)
head(gam_data)



######### 2.2 #############
basic_model = gam(y~x0+s(x1), data= gam_data)
basic_summary = summary(basic_model)
print(basic_summary$p.table)
print(basic_summary$s.table)

plot(basic_model)


######### 2.3 #############
two_term_model = gam(y~x0+s(x1)+x2, data= gam_data)
two_term_summary = summary(two_term_model)
print(two_term_summary$p.table)
print(two_term_summary$s.table)

plot(two_term_model)


######### 2.4 #############
two_smooth_model = gam(y~x0+s(x1)+s(x2), data= gam_data)
two_smooth_summary = summary(two_smooth_model)
print(two_smooth_summary$p.table)
print(two_smooth_summary$s.table)

plot(two_smooth_model,page=1)


######### 2.5 #############
anova(basic_model,two_term_model,two_smooth_model,
      test="Chisq")



####################################
######### 3: Interactions ##########
####################################

######### 3.1 #############
catagorical_interact = gam(y~x0+s(x1)+s(x2,by=x0), 
                           data= gam_data)
catagorical_interact_summary = summary(catagorical_interact)
print(catagorical_interact_summary$s.table)
plot(catagorical_interact,page=1)
anova(two_smooth_model,catagorical_interact,test="Chisq")


######### 3.2 #############
smooth_interact = gam(y~x0+s(x1,x2), 
                      data= gam_data)
smooth_interact_summary = summary(smooth_interact)
print(smooth_interact_summary$s.table)
plot(smooth_interact,page=1)
anova(two_smooth_model,smooth_interact,test="Chisq")


####################################
######## 4: Changing basis #########
####################################

######### 4.1 #############
data(nottem)
n_years = length(nottem)/12
nottem_month = rep(1:12, times= n_years)
nottem_year = rep(1920:(1920+n_years-1),each=12)
nottem_plot = qplot(nottem_month,nottem, 
                    colour=factor(nottem_year), 
                    geom="line")
print(nottem_plot)

######### 4.2 #############
year_gam = gam(nottem~s(nottem_year)+
                 s(nottem_month, bs = "cc"))
print(year_gam$s.table)
plot(year_gam,page=1, scale=0)