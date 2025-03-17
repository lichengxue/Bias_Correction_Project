lib.dir = "C:/Program Files/R/R-4.3.2/library/single_wham"
library(wham,lib.loc = lib.dir)
library(dplyr)
dir <- "C:/Users/spaswham/Desktop/Bias_correction"
setwd(dir)
asap3 <- read_asap3_dat("haddock.dat")

bias_correct_process     <- c(TRUE,FALSE)
bias_correct_observation <- c(TRUE,FALSE)
bias_correct_BRPs        <- c(TRUE,FALSE)
df.mods <- expand.grid(bias_correct_process = bias_correct_process, 
                       bias_correct_observation = bias_correct_observation, 
                       bias_correct_BRPs = bias_correct_BRPs, 
                       stringsAsFactors = FALSE)

n.mods <- dim(df.mods)[1] #8 scenarios!
df.mods$Model <- paste0("m_",1:n.mods)
df.mods <- df.mods %>% select(Model, everything()) # moves Model to first col
nsim = 100
sim_input = list()
for(m in 1:n.mods){
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
                              NAA_re = list(sigma="rec+1", cor="iid"),
                              age_comp = "logistic-normal-miss0",
                              model_name=df.mods$Model[m],
                              basic_info = list(
                                bias_correct_process = df.mods$bias_correct_process[m],
                                bias_correct_observation = df.mods$bias_correct_observation[m],
                                bias_correct_BRPs = df.mods$bias_correct_BRPs[m], #bias correct BRPs when NAA RE are bias-corrected
                                simulate_process_error = rep(TRUE, 5),
                                #XSPR_R_avg_yrs = 50, #only need 1 year because expected recruitment is constant
                                XSPR_R_opt = 5)
  )
  mod <- fit_wham(input, do.fit = TRUE, do.osa = F, do.retro = F) 
  saveRDS(mod, file.path(dir, paste0("om",m,".RDS")))
  #simulate data from operating model
  set.seed(12345) #use same seed for all operating models?
  #all RE and data are simulated
  sim_input[[m]] = lapply(1:nsim, function(x) {
    input_i = input
    sim = mod$simulate(complete=TRUE)
    input_i$data = sim
    return(input_i)
  })
}
saveRDS(sim_input, file.path(dir, "om_sim_data.RDS"))

em_input = list()
sim_input <- readRDS(file.path(dir, "om_sim_data.RDS"))
for(m in 1:n.mods){ # om number 
  em_input = list()
  for (n in 1:n.mods) { # em number
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
                                NAA_re = list(sigma="rec+1", cor="iid"),
                                age_comp = "logistic-normal-miss0",
                                model_name=df.mods$Model[n],
                                basic_info = list(
                                  bias_correct_process = df.mods$bias_correct_process[n],
                                  bias_correct_observation = df.mods$bias_correct_observation[m],
                                  bias_correct_BRPs = df.mods$bias_correct_BRPs[n], #bias correct BRPs when NAA RE are bias-corrected
                                  simulate_process_error = rep(TRUE, 5),
                                  #XSPR_R_avg_yrs = 50, #only need 1 year because expected recruitment is constant
                                  XSPR_R_opt = 5)
    )
    em_input[[n]] = lapply(1:nsim, function(x) {
      input_i = input
      input_i$data = sim_input[[n]][[x]]$data #put in simulated operating model data
      return(input_i)
    })
  }
  saveRDS(em_input, paste0("em_input",'_OM_',m,".RDS"))
}

# Option for parallel computation
Parallel = T

if (Parallel) {
  library(doParallel)
  myCluster <- makeCluster(60, # number of cores to use
                           type = "PSOCK") # type of cluster
  print(paste0("Number of Cores: ",detectCores()))
  print(paste0("Cores Registered: ",myCluster))
  registerDoParallel(myCluster)
}

for(m in 1:n.mods) {
  em_input <- readRDS(paste0("em_input",'_OM_',m,".RDS"))
  for (n in 1:n.mods) {
    foreach(k = 1:100) %dopar% {
      lib.dir = "C:/Program Files/R/R-4.3.2/library/single_wham"
      library(wham,lib.loc = lib.dir)
      out = try(fit_wham(em_input[[n]][[k]], do.osa = FALSE, MakeADFun.silent = TRUE, retro.silent = TRUE, save.sdrep = FALSE))
      saveRDS(out, paste0("Res_",'OM_',m,'_EM_',n,"_nsim_",k,".RDS"))
    }
  }
}



