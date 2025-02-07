########## 5 - Calculation of the Mean Square Error (MSE) between the density of presences (actual response)
##                                           and the response curves from the model (modeled response) ##########


rm(list = ls())

### Set the working directory to the path where you downloaded the "Example_Virtual_Species" folder

setwd("./Example_Virtual_Species/Models")
model =  dir(path = ".")

pred_total = data.frame(X = rep(NA,5))

for (h in 1:length(model)){

  setwd(paste0("./", model[h]))

  pres = read.csv("Presences_norm_inter_VS.csv")
  pres[is.na(pres)] = 0

  rc = read.csv("RC_VS.csv")

  pred = data.frame()

  for (i in seq(1,10,2)){

    x = pres[,i]
    y1 = pres[,i+1]
    y2 = rc[,i+1]

    mse = c()

    for (j in 1:nrow(pres)){

      mse_step = (y1[j] - y2[j])^2

      mse = rbind(mse, mse_step)
    }

    mse = sum(mse, na.rm = T) / nrow(pres)

    # plot them out

    png(paste0("MSE_Presences_RC_", colnames(pres)[i],".png"))

    plot(x,y1, type = "l", bty = "l", lwd = 2, las = 1, ylab = "y")
    lines(x,y2, col = "red", lwd = 2)
    title(main = paste0("Simil = ", round(mse, 3)))

    dev.off()

    pred = rbind(pred, mse)

  }

  rownames(pred) = c("SBT", "SST", "Salinity", "Ice", "Wind")

  colnames(pred) = model[h]

  write.csv(pred, "MSE_Presences_RC.csv", row.names = T)

  pred_total = cbind(pred_total, pred)

  setwd("..")
}


pred_total = pred_total[,-c(1)]
write.csv(pred_total, "MSE_Presences_RC_Every_Models.csv", row.names = T)
