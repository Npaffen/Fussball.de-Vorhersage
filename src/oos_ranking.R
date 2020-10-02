
## calculate the rank correlation between simulated and real table results


# for each season
seasons <- c("1617", "1718", "1819")


# output table
ranking_results <- setNames(data.frame(matrix(ncol = 4, nrow = 0)),
                            c("method", "season", "spearmans_rho", "kendalls_tau"))



# calculate correlation values for each simulation
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
      data_sim <- readRDS(paste0("data/elo_ties_simulation_", i, ".rds"))
    } else if(j == "points"){
      data_sim <- readRDS(paste0("data/point_simulation_", i, ".rds"))
    } else {
      data_sim <- readRDS(paste0("data/poisson_score_simulation_", i, ".rds"))
      
    } 
    rank_sim <- dplyr::slice(data_sim$all_avg_tables, -1:-(dplyr::n()-17))
    compare <- merge(rank_real, rank_sim, by = "club_name")
    names(compare)[2:3] <- c("points.x", "points.y")
    
    # compute correlation
    results$spearmans_rho <- cor.test(compare$points.x, compare$points.y, method = "spearman")$estimate
    results$kendalls_tau <- cor.test(compare$points.x, compare$points.y, method = "kendall")$estimate
    
    # save results
    ranking_results <- rbind(ranking_results, results)
    
    print(paste0(i, j))
  }
}

# summarize results over method
sum_output <- dplyr::summarize(dplyr::group_by(ranking_results, method),
                 spearmans_rho = mean(spearmans_rho),
                 kendalls_tau = mean(kendalls_tau)
                 )

# make latex tables
output <- capture.output(print(xtable::xtable(sum_output,
                                              caption = "Average rank correlation coefficients for simulation and actual data"),
                               include.rownames = FALSE
                               )
                         )
append.output <- capture.output(print(xtable::xtable(ranking_results,
                                              caption = "Rank correlation coefficients for simulation and actual data"),
                               include.rownames = FALSE
                               )
                         )
cat(paste(output, collapse = "\n"), "\n", file="paper/rank_corr.tex", append=FALSE)
cat(paste(append.output, collapse = "\n"), "\n", file="paper/append_rank_corr.tex", append=FALSE)

