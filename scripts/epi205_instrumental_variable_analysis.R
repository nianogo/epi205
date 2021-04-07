#------------------------------------------------------------------------------#
#-----------------------Instructor: Dr Roch Nianogo ---------------------------#
#-----------------------------Date:04/07/21------------------------------------#
#-------Purpose: to illustrate instrumental variable analysis------------------#
#------------------------------------------------------------------------------#



#------------------------------------Checking the directory---------------------

getwd()


#---------------------------------Mise en place---------------------------------
mise()
#remove(list=ls())


#-------------------------------------Installing packages-----------------------

if (!require("pacman")){
  install.packages("pacman", repos = 'http://cran.us.r-project.org')
  }
#general-purpose packages
p_load("tidyverse", # Data managment
       "here")      # directory managment

#------------------------------------------------------------------------------#
#------------------------------SIMULATING DATA----------------------------------
#------------------------------------------------------------------------------#

set.seed(123)
n=1e5
iv <- tibble(
  id=1:n,
  z= rbinom(n, size=1, prob=.4),
  w = rbinom(n, size=1, prob=0.1 + 0.6*z),
  u= rbinom(n,size=1, prob=0.4),
  c= rbinom(n,size=1, prob=0.6),
  x=  rbinom(n=n,size=1, 0 + 0.8*z + 0.1*u + 0.1*c),
  y = rnorm(n, mean=4 + 2*x + 3*c + 6*u, sd=1),
  perprotocol = case_when(((z==1 & x==0) | (z==0 & x==1)) ~0,
                          TRUE~1)
)

View(iv)
glimpse(iv)

#checking the correlation between instrument z and x
cor(iv$x,iv$z, method="spearman")
mxz <- glm(x ~ z, family=gaussian("identity"), data=iv)
tidy(mxz)

#--------------------------------------Analysis---------------------------------
p_load("broom") #tidying up regression output

#True model
true_model <- glm(y ~ x + c + u, family=gaussian("identity"), data=iv)
tidy(true_model)

#As treated
as_treated_model <- glm(y ~ x + c, family=gaussian("identity"), data=iv)
tidy(as_treated_model)

#Per protocol
per_protocol_model <- glm(y ~ x + c, family=gaussian("identity"),
                        data=iv %>% filter(perprotocol==1))
tidy(per_protocol_model)


#Intent-to-Treat (ITT)
itt_model <- glm(y ~ z + c, family=gaussian("identity"), data=iv)
tidy(itt_model)
#ITT = ATE if compliance is perfect but discounted/diluted due to noncompliers

# if z is perfectly randomized and there is no interaction
# then U is no longer a confounder since X is perfectly correlated with Z
# If Z is not perfectly randomized, then ITT would be lower than that the actual treatment effect



#IV analysis
#Wald estimator
#--------------#
#typically done with aggregated data
itt_model <- glm(y ~ z + c, family=gaussian("identity"), data=iv)
tidy(itt_model)
est <- tidy(itt_model)
theta_z = est$estimate[2]

mxz_model <- glm(x ~ z + c, family=gaussian("identity"), data=iv) 
tidy(mxz_model)
estxz <- tidy(mxz_model)
alpha_z = estxz$estimate[2]

betax = theta_z/alpha_z
betax

#Two stage least squares estimation (gold standard)
#-------------------------------------------------#
#Method 1
#First stage
mxz <- glm(x ~ z, family=binomial("logit"), data=iv) 
iv$pxz <- predict(mxz, type="response")

#Second stage
my <- glm(y~ pxz, family=gaussian("identity"), data=iv)
tidy(my)
tidy(coeftest(my, vcov=vcovHC(my, type="HC1"))) #for robust SE


#Method 2
#First stage
mxz <- glm(x ~ z + c, family=binomial("logit"), data=iv) 
iv$pxz <- predict(mxz, type="response")

#Second stage
my <- glm(y~ pxz + c, family=gaussian("identity"), data=iv)
tidy(my)
res <- tidy(coeftest(my, vcov=vcovHC(my_norm, type="HC1"))) #for robust SE
res
p_load("knitr","kableExtra") # for creating tables
kable_classic(kable(res, 
                    digits=4, 
                    align='c',
                    caption="Second stage in the 2SLS model for the 'wage' equation"))

#Method 3.The IV reg procedure
p_load("AER")
model_ivreg <- ivreg(y ~ x + c| z + c, data = iv)
tidy(model_ivreg)
#if you add a auxilliary variable c in the instrument model, you also
#need to add it back to the y model otherwise it will be biased
tidy(coeftest(model_ivreg, vcov=vcovHC(model_ivreg, type="HC1")))


#Method 4: using w as an instrument
#First stage
mxw <- glm(x ~ w, family=binomial("logit"), data=iv) #use logit for binary otherwise it may not converge
tidy(mxw)
iv$pxw <- predict(mxw, type="response")

#Second stage
my <- glm(y~ pxw, family=gaussian("identity"), data=iv)
tidy(my)
tidy(coeftest(my, vcov=vcovHC(my, type="HC1"))) #for robust SE


#Testing for weak instruments
#----------------------------#
model_ivreg <- ivreg(y ~ x + c| z + w + c, data = iv)
tidy(model_ivreg)

summary(model_ivreg, diagnostics=TRUE)
#Weak instruments test: rejects the null (H0: all instruments are weak),
       #meaning that at least one instrument is strong

# (Wu-)Hausman test for endogeneity: rejects the null that the instrument is
# uncorrelated with the error term

# Sargan overidentifying restrictions test: does not reject the null, 
# meaning that the extra instruments are valid (are uncorrelated with the error term).

#--------------------------------Drawing a DAG----------------------------------
p_load("ggdag")

coords <- list(
  y = c( 
    z = 1, 
    x = 1,
    u = 1.5,
    c = 1.5,   
    y = 1
  ),
  x = c( 
    z = 1, 
    x = 2,
    u = 3,
    c = 4,   
    y = 5
  )
)

dag0 <- dagify(y ~ u + c + x,
               x ~ z + u + c,
               coords=coords) 

ggdag(dag0, node_size = 18) + 
  geom_dag_point(color="white") +
  geom_dag_text(color="black") +
  theme_dag()

#------------------------------------------------------------------------------#
#----------------------------------PANEL DATA-----------------------------------
#------------------------------------------------------------------------------#

#------------------------Loading and Exploring the data-------------------------
mydata <- read_csv(here("data", "epi205.csv")) 

#exploring the data
#-----------------#
glimpse(mydata)
View(mydata)

p_load("Hmisc")
descr <- describe(mydata)
descr


#Making a table
#-------------#
p_load("gtsummary")

n_treated0 <- mydata %>% 
  filter(treated == 0) %>% 
  select(state) %>% 
  n_distinct()

n_treated1 <- mydata %>% 
  filter(treated == 1) %>% 
  select(state) %>% 
  n_distinct()

tab1 <- mydata %>%
  filter(post==0) %>% 
  select(c("xit","xt", "xi", "treated","y")) %>% 
  mutate(treated= case_when(treated==1~"Treated",
                            TRUE~"Control")) %>% 
  tbl_summary(
    missing = "no",
    by ="treated",
    type = list(everything() ~ "continuous"),
    digits = list(everything() ~ 2),
    statistic = list(everything()~"{mean}")
  ) %>% 
  add_p(test= list(everything() ~ "t.test")) %>% 
  modify_spanning_header(c("stat_1", "stat_2") ~ "My first table") %>% 
  modify_header(update = list(
    label ~ '**Characteristic**',
    stat_1 ~ '**Control**, N = {n_treated0}',
    stat_2 ~ '**Treated**, N = {n_treated1}',
    p.value ~ '**p-value**'
  )) 

tab1
tab1 %>% as_flex_table()
tab1 %>% as_hux_table()

#Viewing the data
#----------------#
p_load("panelView")
panelView(y ~ treatedpost, data = as.data.frame(mydata),  index = c("state","year"), pre.post = TRUE) 


#Plotting the data
#----------------#
p0 <- mydata %>% 
  group_by(year, treated) %>% 
  summarise(y=mean(y),.groups="keep") %>% 
  ggplot(aes(x=year, y=y, group=treated, color = factor(treated))) + 
  labs(title = paste("Outcome by year"),
       x = "Year", 
       y = "Outcome",
       colour = "Treatment") +
  geom_line() +
  scale_color_discrete(labels=c("Control", "Treated")) +
  geom_vline(xintercept = 40, lty=2) +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5)) 
p0


#----------------------------Instrumental variable analysis---------------------
p_load("lme4") #for multilvel modeling
#FIRST STAGE
mxzc <- glmer(treatedpost ~ post + year + xit + (1| state) + (1 | year),
              data = mydata,
              family = binomial("logit"))

summary(mxzc)
mydata$pxzc <- fitted(mxzc)
#mydata$pxzc <- predict(mxzc, type="response") #same as above

#SECOND STAGE
p_load("lmerTest") # to see p-values
mypxg <- lmerTest::lmer(y ~ pxzc + year + xit + (1| state) + (1 | year),
                        data=mydata)
summary(mypxg)
p_load("lmtest", "sandwich") #For robust SE
coeftest(mypxg, vcov=vcovHC(mypxg, type="HC1"))


