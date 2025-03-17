setwd("C:/Users/chengxue.li/Desktop/Bias_correction_project")
library(wham)
library(dplyr)
library(kableExtra)
source("set_NAA.R");
library(foreach)
library(doParallel)

subDir <- "haddock_full"

if (file.exists(subDir)){
} else {
  dir.create(file.path(subDir))
}

asap3 <- read_asap3_dat("haddock.dat")

###############################################################################
# assume you have rec only and bias correction off in OM

input <- prepare_wham_input(asap3, 
                            recruit_model=2,
                            selectivity=list(model=rep("age-specific",5), 
                                             re=rep("none",5), 
                                             initial_pars=list(c(0.01, 0.1, 0.3, 1, 1, 1, 1, 1, 1),
                                                               c(0.01, 0.1, 0.3, 0.5, 0.8, 0.9, 1, 1, 1),
                                                               c(0.01, 0.1, 0.3, 0.5, 0.8, 0.9, 1, 1, 1),
                                                               c(0.01, 0.1, 0.3, 1, 1, 1, 1, 1, 1),
                                                               c(0.01, 0.1, 0.3, 0.5, 0.8, 1, 1, 1, 1)), 
                                             fix_pars=list(4:9,7:9,7:9,4:9,6:9)),
                            NAA_re = list(sigma="rec", cor="iid"),
                            age_comp = "logistic-normal-miss0",
                            basic_info = list(
                              bias_correct_process = FALSE,
                              bias_correct_observation = FALSE,
                              simulate_process_error = rep(TRUE, 5),
                              XSPR_R_opt = 5)
)

# generating OM with pe and oe on and off (should have 4 different mods)
bias_correct_process     <- c(1,0)
bias_correct_observation <- c(1,0)
sigma <- c("rec","rec+1")
cor <- c("iid","ar1_y")

df.mods1 <- expand.grid(bias_correct_process = bias_correct_process, 
                        bias_correct_observation = bias_correct_observation, 
                        sigma = sigma,
                        cor = cor,
                        stringsAsFactors = FALSE)

bias_correct_process     <- c(1,0)
bias_correct_observation <- c(1,0)
sigma <- c("rec+1")
cor <- c("ar1_a","2dar1")

df.mods2 <- expand.grid(bias_correct_process = bias_correct_process, 
                        bias_correct_observation = bias_correct_observation, 
                        sigma = sigma,
                        cor = cor,
                        stringsAsFactors = FALSE) 

df.mods <- rbind(df.mods1,df.mods2)

for (i in 1:nrow(df.mods)){ 
  
  input_om <- input 
  input_om$data$bias_correct_pe <- df.mods[i, 1]
  input_om$data$bias_correct_oe <- df.mods[i, 2]
  
  NAA_re <- list(sigma = df.mods[i, 3], cor = df.mods[i, 4])
  input_om <- set_NAA(input_om, NAA_re = NAA_re)
  
  om <- try(fit_wham(input_om, do.fit = T, do.osa = FALSE, do.retro = FALSE))
  saveRDS(om,file.path(subDir,paste0("OM",i,".RDS")))
  
  om_input <- om$input
  om_input$par <- om$parList
  om <- fit_wham(om_input, do.fit = F, do.osa = F, do.retro = F) 
  om$fn()
  
  # saveRDS(om,file.path(subDir,paste0("OM",i,".RDS")))
  
  em_results <- list()
  
  set.seed(123)
  
  for (j in 1:100) {
    sims = om$simulate(complete=T)
    em_results[[j]] <- list()
    res <- list()
    for (k in 1:4){
      input_em <- om$input
      input_em$data <- sims
      input_em$data$bias_correct_pe <- df.mods[k,1] 
      input_em$data$bias_correct_oe <- df.mods[k,2] 
      mod <- try(fit_wham(input_em, do.fit = F, do.osa = F, do.retro = F))
      mod$fn()
      res[[k]] <- mod
    }
    em_results[[j]] <- res
  }
  saveRDS(em_results,file.path(subDir,paste0("OM",i,"EM.RDS")))
}