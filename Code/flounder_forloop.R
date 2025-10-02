library(wham)
library(dplyr)
library(kableExtra)
source("set_NAA.R")
library(foreach)
library(doParallel)

subDir <- "GBK_full"

if (file.exists(subDir)){
} else {
  dir.create(file.path(subDir))
}

# load data (note: I downloaded GBK.DAT from Google Drive on 11/30/2023, changed the first year to 1973 in ASAP, and saved it as GBK1973.DAT)
gb_dat <- read_asap3_dat("GBK1973.DAT")

gb_dat$dat$IAA_mats[[3]] # problem with missing ESS for DFO survey
# set to 25
gb_dat$dat$IAA_mats[[3]][c(15:49),10]<-25
gb_dat$dat$IAA_mats[[3]]

sel=list(
  model=rep("logistic",4), 
  re = c("none","none","none","none"),
  initial_pars=list(
    c(2,0.3), # Commercial fleet
    c(2,0.3), # Spring NEFSC
    c(2,0.3), # Fall NEFSC
    c(2,0.3)), # DFO survey
  fix_pars = list(
    c(NULL),
    c(NULL),
    c(NULL),
    c(NULL))
)

#bias correction as usual
input <- prepare_wham_input(gb_dat, 
                            selectivity=sel,
                            age_comp = "dirichlet-miss0",
                            NAA_re = list(sigma="rec", cor ="iid"),
                            basic_info = list(
                              bias_correct_process = FALSE,
                              bias_correct_observation = FALSE,
                              #bias_correct_BRPs = FALSE, #bias correct BRPs when NAA RE are bias-corrected
                              simulate_process_error = rep(TRUE, 5),
                              XSPR_R_avg_yrs = 50, #only need 1 year because expected recruitment is constant
                              XSPR_R_opt = 5) #uses bias-corrected expected recruitment
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

do.parallel = TRUE

n.rep = 60 # It depends how many cores/workers
# Option for parallel computation
if (do.parallel) {
  library(doParallel)
  myCluster <- makeCluster(n.rep) # type of cluster
  print(paste0("Number of Cores: ",detectCores()))
  print(paste0("Cores Registered: ",myCluster))
  registerDoParallel(myCluster)
}

for (i in 24:nrow(df.mods)){ 
  
  input_om <- input 
  input_om$data$bias_correct_pe <- df.mods[i, 1]
  input_om$data$bias_correct_oe <- df.mods[i, 2]
  
  NAA_re <- list(sigma = df.mods[i, 3], cor = df.mods[i, 4])
  input_om <- set_NAA(input_om, NAA_re = NAA_re)
  
  om <- try(fit_wham(input_om, do.fit = T, do.osa = FALSE, do.retro = FALSE))
  saveRDS(om,file.path(subDir,paste0("OM",i,".RDS")))
  
  foreach (j = 1:50) %dopar% {
    
    library(wham)
    set.seed(123+j)
    sims = om$simulate(complete=T)
    
    res <- list()
    for (k in 1:4){
      input_em <- om$input
      input_em$data <- sims
      input_em$data$bias_correct_pe <- df.mods[k,1] 
      input_em$data$bias_correct_oe <- df.mods[k,2] 
      mod <- try(fit_wham(input_em, do.fit = T, do.osa = F, do.retro = F))
      if(class(mod) == "try-error") {
        mod <- list()
      } else {
        mod$fn()
      }
      res[[k]] <- mod
    }
    saveRDS(res,file.path(subDir,paste0("OM",i,"nsim",j,".RDS")))
  }
}
