
  setwd("C:/Users/muir/Documents/BUAD466")

# install.packages("lme4")
  library(lme4)

  conjoint.df <- read.csv(file="conjoint.csv",stringsAsFactors=TRUE)

  str(conjoint.df)
  head(conjoint.df,n=20)

  conjoint.df$speed <- factor(x=conjoint.df$speed)
  conjoint.df$height <- factor(x=conjoint.df$height)

  str(conjoint.df)

### fixed effects model ONLY

  ride.lm <- lm(formula=rating~speed+height+const+theme,data=conjoint.df)
  summary(ride.lm)

# Coefficients:
#             Estimate Std. Error t value Pr(>|t|)    
# (Intercept)  3.07307    0.08102  37.932  < 2e-16 ***
# speed50      0.82077    0.10922   7.515 7.35e-14 ***
# speed60      1.57443    0.12774  12.326  < 2e-16 ***
# speed70      4.48697    0.15087  29.740  < 2e-16 ***
# height300    2.94551    0.09077  32.452  < 2e-16 ***
# height400    1.44738    0.12759  11.344  < 2e-16 ***
# constWood   -0.11826    0.11191  -1.057    0.291    
# themeEagle  -0.75454    0.11186  -6.745 1.81e-11 ***

### fixed + random effects model with varying intercept ONLY

  ride.hlml <- lmer(formula=rating~speed+height+const+theme+(1|resp.id),
                    data=conjoint.df)
  summary(ride.hlml)

# Fixed effects:
#             Estimate Std. Error t value
# (Intercept)  3.07307    0.08759  35.084
# speed50      0.82077    0.10439   7.862
# speed60      1.57443    0.12209  12.895
# speed70      4.48697    0.14421  31.115
# height300    2.94551    0.08676  33.951
# height400    1.44738    0.12195  11.868
# constWood   -0.11826    0.10696  -1.106
# themeEagle  -0.75454    0.10692  -7.057

# These are the same as the fixed effects from the linear model above.

# Random effects:
#  Groups   Name        Variance Std.Dev.
#  resp.id  (Intercept) 0.3352   0.5789  # <-- The variation in the (Intercept)
#  Residual             3.5358   1.8804  #     is 1/2 rating point.   
# Number of obs: 3200, groups:  resp.id, 200

# Average (Intercept) = 2.5 = .5789 - 3.07307 + .5789 = 3.65 rating points
# +/- 1 SD in a normal distribution

# What is captured in the (Intercept)? 
# crosses the "DV-axis"+speed40+height200+constSteel+themeDragon

# Total number of possible combinations: 4*3*2*2 = 48
# 200 individual-level random effects were estimated in the HLM

# Total number of coefficients = fixed effects coefficients [8] +
#                                random effects coefficients [200] = 208

  fixef(ride.hlml)
  ranef(ride.hlml)$resp.id

# Total individual-level effect = population-level fixed effect +
#                                 individual-level random effect

  coef(ride.hlml)$resp.id
  fixef(ride.hlml)+ranef(ride.hlml)$resp.id # [intercept only]

#              fixed    +  random effect = complete/total effect
# (Intercept)  3.07307  +  random effect = complete/total effect
# speed50      0.82077  +  0             = complete/total effect
# speed60      1.57443  +  0
# speed70      4.48697  +  0
# height300    2.94551  +  0
# height400    1.44738  +  0
# constWood   -0.11826  +  0
# themeEagle  -0.75454  +  0             = complete/total effect

# PERSON 1
# (Intercept) fixed effect  =  3.07307
# (Intercept) random effect = -0.65085635
# (Intercept) total effect  =  2.42221607

# PERSON 2
# (Intercept) fixed effect  =  3.07307
# (Intercept) random effect = -0.04821158
# (Intercept) total effect  =  3.024861

### fixed + random effects model with ALL varying effects

# install.packages("optimx")
  library(optimx)

  methods <- c("Nelder-Mead","BFGS","CG","L-BFGS-B","nlm","nlminb","spg",
               "ucminf","newuoa","bobyqa","nmkb","hjkb","Rcgmin","Rvmmin")
  message <- rep(NA,length.out=14)
  for(i in 1:length(methods)) {
    ride.chlml <- lmer(formula=rating~speed+height+const+theme+
                       (speed+height+const+theme|resp.id),
                       data=conjoint.df,REML=TRUE,
                       control=lmerControl(optimizer="optimx",
                                           optCtrl=list(method="L-BFGS-B")))
    message[i] <- ride.chlml@optinfo$conv$lme4$messages
    }
  summary(ride.chlml)

# Total number of coefficients = fixed effects coefficients [8] +
#                                random effects coefficients [Intercept  200] +
#                                                            [speed50    200] +
#                                                            [speed60    200] +
#                                                            [speed70    200] +
#                                                            [height300  200] +
#                                                            [height400  200] +
#                                                            [constWood  200] +
#                                                            [themeEagle 200]
# Total number of coefficients = 1,608 = 8 + 200*8

# N = 3,200 observations from n = 200 people --> squeeze out 1,608 coefficients
# 3,200/200 = 16 observations per person --> # survey questions per person

# Linear mixed model fit by REML ['lmerMod']
# Formula: rating ~ speed + height + const + theme + (speed + height + const +  
#     theme | resp.id)
#    Data: conjoint.df
# Control: lmerControl(optimizer = "optimx", optCtrl = list(method = "L-BFGS-B"))
#
# REML criterion at convergence: 12797.4
# 
# Scaled residuals: 
#     Min      1Q  Median      3Q     Max 
# -3.2282 -0.6058 -0.0061  0.5697  3.6300 
#
# Random effects:
#  Groups   Name        Variance Std.Dev. Corr                                     
#  resp.id  (Intercept) 1.85258  1.3611                                            
#           speed50     0.09238  0.3039    0.26                                    
#           speed60     0.20292  0.4505   -0.08 -0.04                              
#           speed70     0.25641  0.5064    0.14 -0.33 -0.59                        
#           height300   0.25269  0.5027   -0.27  0.27  0.46 -0.88                  
#           height400   0.65040  0.8065   -0.22  0.69  0.25 -0.26  0.45            
#           constWood   2.32281  1.5241   -0.64 -0.19 -0.12 -0.03  0.05 -0.05      
#           themeEagle  1.64031  1.2807   -0.52 -0.02  0.34 -0.16  0.06  0.23 -0.19
#  Residual             2.32149  1.5236                                            
# Number of obs: 3200, groups:  resp.id, 200
#
# Fixed effects:
#             Estimate Std. Error t value
# (Intercept)  3.07307    0.11489  26.748
# speed50      0.82077    0.08728   9.404
# speed60      1.57443    0.10393  15.148
# speed70      4.48697    0.12221  36.715
# height300    2.94551    0.07877  37.392
# height400    1.44738    0.11409  12.686
# constWood   -0.11826    0.13830  -0.855
# themeEagle  -0.75454    0.12533  -6.020
#
# Correlation of Fixed Effects:
#            (Intr) sped50 sped60 sped70 hgh300 hgh400 cnstWd
# speed50    -0.144                                          
# speed60    -0.104  0.592                                   
# speed70    -0.018  0.423  0.496                            
# height300  -0.308 -0.239 -0.333 -0.442                     
# height400  -0.181 -0.360 -0.302 -0.182  0.433              
# constWood  -0.491 -0.302 -0.249 -0.173  0.149  0.279       
# themeEagle -0.391  0.120 -0.015 -0.212  0.132 -0.128 -0.353
# optimizer (optimx) convergence code: 1 (none)
# boundary (singular) fit: see help('isSingular')

  coef(ride.chlml)$resp.id
  fixef(ride.chlml)+ranef(ride.chlml)$resp.id

#              average    1 std dev
# constWood   -0.11826 +/- 1.5241
# constWood   [-1.64236, 1.40584]

# Does this mean individuals on average have no preference or are
# indifferent between wood and steel coasters?
# It depends whether the distribution of random effects is concentrated
# at the average/fixed effects, or if the distribution of random effects
# is widely distributed.

  rideConstWood <- fixef(ride.chlml)["constWood"]+ranef(ride.chlml)$resp.id[,"constWood"]
  hist(x=rideConstWood,xlim=c(-5,5),
       xlab="Rating Points",ylab="Frequency of Respondents",
       main="Respondent Preferences for Wood v. Steel Construction")

  rideSpeed70 <- fixef(ride.chlml)[4]+ranef(ride.chlml)$resp.id[,4]
  hist(x=rideSpeed70,xlim=c(-3,7),
       xlab="Rating Points",ylab="Frequency of Respondents",
       main="Respondent Preferences for Speed of 70mph v. 40mph")
