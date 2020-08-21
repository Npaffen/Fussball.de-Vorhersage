
## calculate the oos ranking score by comparing simulation results and real


# for each season
seasons <- c("1617", "1718", "1819")


# output table
ranking_results <- setNames(data.frame(matrix(ncol = 4, nrow = 0)),
                            c("method", "season", "spearmans_rho", "kendalls_tau"))




for(j in c("elo ranking", "points", "poisson")){

    for(i in seasons){

    results <- ranking_results[nrow(ranking_results)+1,]
    results$season <- i
    results$method <- j
    
    # load real data
    data_real <- readRDS(paste0("data/database_season_", i, ".rds"))
    rank_real <- data_real[data_real$matchday==max(data_real$matchday),]
    rank_real <- rank_real[,c("club_name", "points")]
    
    # load simulated data
    if(j == "elo ranking"){
      data_sim <- readRDS(paste0("data/elo_ties_simulation._", i, "rds"))
    } else if(j == "points"){
      data_sim <- readRDS(paste0("data/point_simulation_", i, ".rds"))
    } else {
      data_sim <- readRDS(paste0("data/poisson_score_simulation_", i, ".rds"))
      
    } 
    rank_sim <- dplyr::slice(data_sim$all_avg_tables, -1:-(dplyr::n()-17))
    compare <- merge(rank_real, rank_sim, by = "club_name")
    
    # compute correlation
    results$spearmans_rho <- cor.test(compare$score.x, compare$score.y, method = "spearman")$estimate
    results$kendalls_tau <- cor.test(compare$score.x, compare$score.y, method = "kendall")$estimate

    # save results
    ranking_results <- rbind(ranking_results, results)
    
    print(paste0(i, j))
  }
}

stargazer::stargazer(ranking_results)
