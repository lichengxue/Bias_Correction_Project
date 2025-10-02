library(wham)
library(dplyr)
library(kableExtra)
source("set_NAA.R")
library(foreach)
library(doParallel)

subDir <- "mackerel_full"

if (file.exists(subDir)){
} else {
  dir.create(file.path(subDir))
}

asap3 <- read_asap3_dat("Mack_2019.dat")

###############################################################################
# assume you have rec only and bias correction off in OM

input <- prepare_wham_input(asap3, 
                            recruit_model=2,
                            selectivity=list(model = c("age-specific",
                                                       "age-specific",
                                                       "age-specific",
                                                       "age-specific"), 
                                             initial_pars = list(c(0.1, 0.5, 0.8, rep(1,7)),
                                                                 rep(1,10),
                                                                 c(0, 0, 1, 0.6, 0.4, 0.2, 0, 0, 0, 0),
                                                                 c(0, 0, 1, 0.6, 0.4, 0.2, 0, 0, 0, 0)),
                                             fix_pars = list(c(4:10),1:10,c(1:3,7:10),c(1:3))), 
                            NAA_re = list(sigma="rec", cor="iid"),
                            # age_comp = "dirichlet-pool0",
                            age_comp = "logistic-normal-ar1-miss0",
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
