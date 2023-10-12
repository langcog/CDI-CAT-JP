html_table_width <- function(kable_output, width){
  width_html <- paste0(paste0('<col width="', width, '">'), collapse = "\n")
  sub("<table>", paste0("<table>\n", width_html), kable_output)
} 

get_anova_table <- function(mod1, mod2, model_names=c("model1","model2")) {
  aa = anova(mod1, mod2)
  tab = data.frame(Model=model_names, AIC=aa$AIC, BIC=aa$BIC, logLik=aa$logLik, df=aa$df)
  return(tab)
}

doCAT <- function(dat, mod, max_items=50, min_SEM=.1) {
  all_items = c() # track freq of all the items that were asked 
  parms = c()
  results <- mirtCAT(mo = mod, criteria = 'MI', start_item = 'MI', method = 'MAP', # cl = cl,
                     local_pattern = dat, design = list(max_items = max_items, min_SEM = min_SEM))
  for(s in 1:nrow(dat)) {
    all_items = c(all_items, results[[s]]$items_answered)
    so <- summary(results[[s]])
    parms = rbind(parms, c(t(so$final_estimates), length(so$items_answered)))
  }
  sum_score = rowSums(dat)
  parms = data.frame(cbind(parms, sum_score))
  names(parms) = c("thetaCAT","CAT_SE","Qs_asked","sum_score")
  want = list(all_items=all_items, parms=parms)
  return(want)
}

doCAT_fixed_length <- function(dat, mod, min_items=50, criteria='MI', start_item='MI') {
  all_items = c() # track freq of all the items that were asked 
  parms = c()
  results <- mirtCAT(mo = mod, criteria = criteria, start_item = start_item, method = 'MAP', cl = cl,
                     local_pattern = dat, design = list(min_items = min_items, max_items=min_items))
  for(s in 1:nrow(dat)) {
    all_items = c(all_items, results[[s]]$items_answered)
    so <- summary(results[[s]])
    parms = rbind(parms, c(t(so$final_estimates), length(results[[s]]$items_answered)))
  }
  sum_score = rowSums(dat)
  parms = data.frame(cbind(parms, sum_score))
  names(parms) = c("thetaCAT","CAT_SE","Qs_asked","sum_score")
  want = list(all_items=all_items, parms=parms)
  return(want)
}

summarize_CAT <- function(catdat, d_mat, verbose=F) {
  meanSE = mean(catdat$parms$CAT_SE)
  never_selected = setdiff(1:ncol(d_mat), unique(catdat$all_items))
  num_unused = length(never_selected)
  item_freq = sort(table(catdat$all_items))
  #names(which(item_freq<10))
  mean_Qs_asked = mean(catdat$parms$Qs_asked)
  median_Qs_asked = median(catdat$parms$Qs_asked)
  cond = max(catdat$parms$Qs_asked)
  # correlation with subject's estimated ability on full CDI
  r_cat_full = cor(fscores_2pl$ability, catdat$parms$thetaCAT)
  reliability = 1-mean(catdat$parms$CAT_SE)^2
  # also look at correlation of sum_score?
  #cor(catdat$parms$thetaCAT, catdat$parms$sum_score)
  if(verbose) {
    plot(fscores_2pl$ability, catdat$parms$thetaCAT)
    plot(catdat$parms$thetaCAT, catdat$parms$CAT_SE)
    print("Items that were never selected:")
    print(never_selected)
  }
  return(cbind(cond, median_Qs_asked, mean_Qs_asked, r_cat_full, meanSE, reliability, num_unused))
}

preferredCAT <- function(dat, method='ML', min_SEM=.15, start_item=c()) {
  all_items = c() # track freq of all the items that were asked 
  parms = c()
  if(length(start_item)==0) start_item = 'MI' # otherwise supply age-based
  results <- mirtCAT(mo = mod_2pl, criteria = 'MI', start_item = start_item, 
                     method = method, local_pattern = dat, # cl = cl,
                     design = list(min_items = 25,
                                   max_items = 50, 
                                   min_SEM = min_SEM))
  for(s in 1:nrow(dat)) {
    all_items = c(all_items, results[[s]]$items_answered)
    so <- summary(results[[s]])
    parms = rbind(parms, c(t(so$final_estimates), length(so$items_answered)))
  }
  sum_score = rowSums(dat)
  parms = data.frame(cbind(parms, sum_score))
  names(parms) = c("thetaCAT","CAT_SE","Qs_asked","sum_score")
  want = list(all_items=all_items, parms=parms)
  return(want)
}

get_cor_by_age <- function(d, catdat) {
  d$thetaCAT = catdat$parms$thetaCAT
  cors <- d %>%
    group_by(age_group) %>% 
    summarize(r=cor(ability, thetaCAT))
  return(cors)
}

# accepts residuals(model, type="LD"), returns items with LD strengths at/above assoc_str
# no association = abs(V) < .1 no association, .3 is moderate, and .5+ is strong
get_LD_violations <- function(res, assoc_str = .3) {
  vio = rep(NA, nrow(res))
  for(i in 1:nrow(res)) {
    vio[i] = length(which(abs(res[i,i:ncol(res)])>=assoc_str))
  }
  return(vio)
}

# find item with maximum information at given theta value
get_max_info_item <- function(mod, theta) {
  infos = rep(NA, nrow(coefs_2pl))
  for(i in 1:nrow(coefs_2pl)) {
    infos[i] = iteminfo(extract.item(mod, i), theta)
  }
  return(list(item=which(infos==max(infos)), info=max(infos)))
}


get_item_info_1d <- function(mod, item) {
  ii <- extract.item(mod, item)
  Theta <- matrix(seq(-4,4, by = .1))
  info <- iteminfo(ii, Theta)
  return(sum(info))
}

get_item_info_2d <- function(mod, item) {
  ii <- extract.item(mod, item)
  #Theta <- as.matrix(expand.grid(-4:4, -4:4))
  Theta <- as.matrix(expand.grid(seq(-4,4,by=.5), seq(-4,4,by=.5)))
  info = iteminfo(ii, Theta, degrees=c(45,45)) # equal angle
  info1d = iteminfo(ii, Theta, degrees=c(90,0)) # first dimension only
  info2d = iteminfo(ii, Theta, degrees=c(0,90))
  
  # information matrices
  #iteminfo(ii, Theta, multidim_matrix = TRUE)
  #iteminfo(ii, Theta[1, , drop=FALSE], multidim_matrix = TRUE)
  return(c(sum(info), sum(info1d), sum(info2d)))
}
