#------------------------------------------------------------------------------#
#--------------------------Created by YOUR NAME HERE---------------------------#
#-----------------------------Date:--------------------------------------------#
#------------Purpose: to illustrate various useful functions-------------------#
#------------------------------------------------------------------------------#



#-------------------------------------------------------------#
#--------------------------Creating folders------------------
#-------------------------------------------------------------#
#folders are already created for your convenience
# dir.create("data")
# dir.create("rmd")
# dir.create("figures")
# dir.create("tables")
# dir.create("scripts")
# dir.create("assignments")
#-------------------------------------------------------------#
#----------------------Checking the directory------------------
#-------------------------------------------------------------#
getwd()
library(here)
here()

#-------------------------------------------------------------#
#-----------------------Installing packages------------------
#-------------------------------------------------------------#
#1) Tidyverse: This is the main package that will serve to do data management
#install.packages("tidyverse", dependencies=TRUE)
library("tidyverse")

#2)the here package will help will locating files especially within projects
#install.packages("here", dependencies=TRUE)
library("here")


#-------------------------------------------------------------#
#---------------------------Loading the data------------------
#-------------------------------------------------------------#

mydata <- read_csv(here("data", "epi205.csv")) 


#Exporting data
write_csv(mydata, here("data", "mydata.csv")) 

library("writexl")
writexl::write_xlsx(mydata, here("data", "mydata.xlsx")) 

#-------------------------------------------------------------#
#-----------------------Understanding base R------------------
#-------------------------------------------------------------#
5 + 3
5 - 3
5 / 3
5 ^ 3
5 * (10 - 3)
sqrt(4)

mean_age = 50
mean_age
sd_age = 2
sd_age

result <- 5 + 3
result
print(result)
result <- 5 - 3
result

your_name <- "Alphonsus"
your_name


Result <- "5"
Result
result

class(result)
Result
class(Result)
class(sqrt)
#----test----#

## ------------------------------------------------------------------------
world.pop <- c(2525779, 3026003, 3691173, 4449049, 5320817, 6127700, 6916183)
world.pop

pop.first <- c(2525779, 3026003, 3691173)
pop.second <- c(4449049, 5320817, 6127700, 6916183)
pop.all <- c(pop.first, pop.second)
pop.all

world.pop[2]
world.pop[c(2, 4)] 
world.pop[c(4, 2)] 


## ------------------------------------------------------------------------
length(world.pop)  
min(world.pop)     
max(world.pop)     
range(world.pop)   
mean(world.pop)    
sum(world.pop) / length(world.pop) 

year <- seq(from = 1950, to = 2010, by = 10)
year

seq(to = 2010, by = 10, from = 1950)

seq(from = 2010, to = 1950, by = -10)
2008:2012
2012:2008

names(world.pop) 
names(world.pop) <- year
names(world.pop)
world.pop

#the rep function
var1 <- c(1,3,5,7)
var1
rep(var1, times=4)
rep(var1, each=4)


#------creating a function----
## myfunction <- function(input1, input2, ..., inputN) {
## 
##     DEFINE `output' USING INPUTS
## 
##     return(output)
## }

my.summary <- function(x){ # function takes one input
  s.out <- sum(x)
  l.out <- length(x)
  m.out <- s.out / l.out
  out <- c(s.out, l.out, m.out) # define the output
  names(out) <- c("sum", "length", "mean") # add labels
  return(out) # end function by calling output
}
z <- 1:10
z
my.summary(z)
my.summary(world.pop)



#-------------------------------------------------------------#
#--------------------------Exploring the data------------------
#-------------------------------------------------------------#
glimpse(mydata)
str(mydata)

View(mydata)
mean(mydata$xit)

library("Hmisc")
descr <- Hmisc::describe(mydata)
descr



#-------------------------------------------------------------#
#-------------Labelling variables and values----------------
#-------------------------------------------------------------#

#Labelling package
library("labelled")
library("pacman")
p_load("labelled", "Hmisc")

#vignette: http://127.0.0.1:20894/library/labelled/doc/intro_labelled.html
#vignette: http://127.0.0.1:20894/library/labelled/doc/look_for.html
p_vignette("labelled")



dsn <- mydata %>%
  set_variable_labels(treated    = "Treatment Status") %>%
  set_value_labels(treated        = c("Treated" = 1, "Control" = 0))

#to look for a variable
dsn %>% 
  look_for("treated")

dsn %>% 
  look_for("")


#----------------------------------------------#
#-------------Making a table------------------
#----------------------------------------------#
p_load("gtsummary")
#vignette:http://127.0.0.1:20894/library/gtsummary/doc/tbl_summary.html
#vignette: http://127.0.0.1:20894/library/gtsummary/doc/gtsummary_definition.html

n_treated0 <- mydata %>% 
  filter(treated == 0) %>% 
  select(state) %>% 
  n_distinct()
n_treated0

n_treated1 <- mydata %>% 
  filter(treated == 1) %>% 
  select(state) %>% 
  n_distinct()

n_treated1

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
tab1_xlsx <- tab1 %>% as_tibble()

library("writexl")

write_xlsx(tab1_xlsx, here("tables", "myfirst_table.xlsx"))

library("gt")
tab1_png <- tab1 %>% as_gt()
gtsave(tab1_png, here("tables", "myfirst_table.png"))



#----------------------Viewing the data------------------------
library("panelView")
panelView(y ~ treatedpost, data = as.data.frame(mydata),  index = c("state","year"), pre.post = TRUE) 




#-------------------Figures using the ggplot package----------

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

here("figures", "myfirstplot.png")
ggsave(filename=here("figures", "myfirstplot.png"), plot=p0)
ggsave(here("figures", "myfirstplot.png"))



#----------------------------------------------#
#---------------Making a DAG------------------
#----------------------------------------------#
library("ggdag")

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


library("mise")
mise()
rm(ls=ls())
#-------------------------------------------------------------#
#----------------Other very useful packages--------------------
#--------------------------------------------------------------#

pacman::p_load(
  
  "magrittr",    #for the pipe operator: %>% and %<>%
  "broom",       #for tidying up the results of a regression: tidy()
  "lubridate",   #for manipulating dates: today()
  "haven",       #load stata, SAS, SPSS data
  "readxl",      #load excel data: read_excel()
  "writexl",     #write excel doc: write_xlsx()
  "labelled",    #labelleling the data: set_variable_labels(), set_value_labels()
  
  # Other great packages
  "mise",         #clears environment: mise()
  "here",         #set directory: here()
  "pacman",       #load packages: p_load(), p_vignette()
  "glue",         #replaces paste: glue()
  
  # Making tables
  "knitr",        #create tables: kable()
  "kableExtra",   #create tables: kable_styling()
  "huxtable",     #create tables: as_huxtable()
  "flextable",    #create tables: as_flextable()
  "gtsummary",    #create nice labelled tables: tbl_summary(), as_tibble()
  "gt",
  
  # Enhancing plots
  "scales",        #makes easy to format percent, dollars, comas: percent()
  "ggalt",         #makes easy splines: geom_xsplines()
  "ggeasy",        #applies labels among other things: easy_labs()
  "gridExtra",     #combining plots and tables on plots: grid.arrange(), tableGrob()
  "ggpubr",        #combines plots: ggarrange()
  "ggdag",         #plot a DAG: ggdag()
  
  #For manipulating data
  "fastDummies", #create dummy variables: dummy_cols(),
  "tidyverse"
  #ggplot2 for graphs: ggplot()
  #purrr: for iterations: map()
  #tibble: for making better data.frames: tibble()
  #dplyr: for manipulating data: select(), filter()
  #tidyr: expand_grid(), pivot_wider(), pivot_longer()
  #stringr: for dealing with strings : str_to_lower()
  #readr: for reading csv data: read_csv()
  #forcats: for dealing with factor variables
)


# use pacman::p_vignette("tidyverse") for learning more about the "tidyverse" or
#any other package






