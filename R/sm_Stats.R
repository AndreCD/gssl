#' smStats
#' Calculates many different assessment statistics 
#' 
#' @param obs vector of observed values
#' @param pred vector of predicted values
#' @return a data frame consisting of the stats
#' @keywords 
#' @export
#' @examples
#' no examples yet
#' R.A. Viscarra Rossel 2013

sm_Stats <- function(pred, obs, plot = TRUE,...){
  
  library(epiR)
  library(ggplot2)
  
  if (any(is.na(obs)) | any(is.na(pred))) {
    
    warning("NAs!")
    
    temp.df <- data.frame(pred = pred, obs = obs)
    temp.df <- na.omit(temp.df)
    obs <- temp.df$obs
    pred <- temp.df$pred
    
  }
  
  MSE <- sum(((pred - obs)^2) / length(obs))          # mean square error
  RMSE <- sqrt(MSE)                                   # root mean square error
  ME <- mean(pred - obs)                              # mean error
  SDE <- sd(pred - obs)                               # st dev error
  MSE_bc <- sum(((pred - ME - obs)^2) / length(obs))  # bias corrected MSE
  RMSE_bc <- sqrt(MSE_bc)                             # bias corrected RMSE
  lmod <- lm(pred ~ obs)
  R2 <- as.matrix(summary(lmod)$r.squared)            # R2 <- cor(pred, obs)^2
  R2adj <- as.matrix(summary(lmod)$adj.r.squared)
  r_c <- (epi.ccc(pred, obs))$rho.c[,1]               # Lin's concordace correlation   
  # r_c <- 2 * mean((obs - mean(obs)) * (pred -mean(pred))) / (var(obs) + var(pred) + (mean(obs) - mean(pred))^2)
  RPD <- sd(obs) / RMSE                               # ratio of performance to deviation 
  RPIQ <- quantile(obs)[3] - quantile(obs)[2] / RMSE  # ratio of performance to iq distance
  
  stats <- round(data.frame(R2 = R2, R2adj = R2adj, CCC = r_c, MSE = MSE, 
                            RMSE = RMSE, ME = ME, SDE = SDE, bcMSE = MSE_bc, 
                            bcRMSE = RMSE_bc, RPD = RPD, RPIQ = RPIQ, row.names=NULL), 3)

  if(plot){
      p <- ggplot() +
      geom_point(aes(x = obs, y = pred), size = 2.5, alpha = 0.5) + 
      theme(legend.position = "none") +
      theme_bw(base_size = 16, base_family = "") +
      theme(panel.grid.major = element_blank()) + 
      theme(panel.grid.minor = element_blank()) + 
      geom_abline(intercept = 0, slope=1, colour="black", size=0.5) +
      scale_x_continuous(limits = c(min(c(obs, pred)), max(c(obs, pred)))) +
      scale_y_continuous(limits = c(min(c(obs, pred)), max(c(obs, pred)))) +
      theme(legend.position = "")
      print(p)
  }
  
  return(stats)
}


  
  
