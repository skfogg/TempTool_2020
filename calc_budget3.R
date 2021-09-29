calc_budget3 <- function(sw, lw, sens, lat, hypo = 0){


  if (hypo != 0){
    total <- sum(c(sum(abs(sw)), sum(abs(lw)), sum(abs(sens)), sum(abs(lat)), sum(abs(hypo))))

    sw_gain <- sum(subset(sw, x > 0))/total
    lw_gain <- sum(subset(lw, x > 0))/total
    sens_gain <- sum(subset(sens, x > 0))/total
    lat_gain <- sum(subset(lat, x > 0))/total
    hypo_gain <- sum(subset(hypo, x > 0))/total

    sw_loss <- sum(abs(subset(sw, x < 0)))/total
    lw_loss <- sum(abs(subset(lw, x < 0)))/total
    sens_loss <- sum(abs(subset(sens, x < 0)))/total
    lat_loss <- sum(abs(subset(lat, x < 0)))/total
    hypo_loss <- sum(abs(subset(hypo, x < 0)))/total

    frame <- data.frame(sw = c(sw_gain, sw_loss),
                        lw = c(lw_gain, lw_loss),
                        sensible = c(sens_gain, sens_loss),
                        latent = c(lat_gain, lat_loss),
                        hyporheic = c(hypo_gain, hypo_loss),
                        row.names = c("gain", "loss"))
  }else{
    total <- sum(c(sum(abs(sw)), sum(abs(lw)), sum(abs(sens)), sum(abs(lat))))

    sw_gain <- sum(subset(sw, x > 0))/total
    lw_gain <- sum(subset(lw, x > 0))/total
    sens_gain <- sum(subset(sens, x > 0))/total
    lat_gain <- sum(subset(lat, x > 0))/total

    sw_loss <- sum(abs(subset(sw, x < 0)))/total
    lw_loss <- sum(abs(subset(lw, x < 0)))/total
    sens_loss <- sum(abs(subset(sens, x < 0)))/total
    lat_loss <- sum(abs(subset(lat, x < 0)))/total

    frame <- data.frame(sw = c(sw_gain, sw_loss),
                        lw = c(lw_gain, lw_loss),
                        sensible = c(sens_gain, sens_loss),
                        latent = c(lat_gain, lat_loss),
                        hyporheic = c(0, 0),
                        row.names = c("gain", "loss"))
  }

  return(frame)
}
