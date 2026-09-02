
library(tidyverse)


LargeEnvirDir = "//RAGINGWIENIS/personal_folder/Data/Lab/HoverWii data"
WiiReadDir = "//RAGINGWIENIS/personal_folder/Data/Lab/HoverWii data/Original"
WiiWriteDir = "//RAGINGWIENIS/personal_folder/Data/Lab/HoverWii data/ExtraCrispy(new)"
PlotDir = "//RAGINGWIENIS/personal_folder/Data/Lab/HoverWii data/New Plots"
P2Folder = "//RAGINGWIENIS/personal_folder/Data/Lab/HoverWii data/ExtraCrispy(new)/P2Data"




setwd(P2Folder)

files = list.files(
   path = P2Folder,
   pattern = "^P2T[0-9]+N\\.csv$",
   full.names = F
)

trial_list = list()

for (i in 1:length(files))
{
   a = read.table(files[i], header = TRUE, sep = ",", row.names = NULL)
   index = ((which(files == files[i]) %>% as.numeric() )+1) %>% as.character()
   trial_list[[paste0("P2T",index)]] = a 
}

n = length(trial_list)
rms_df = data.frame(
   trial = names(trial_list),
   n_samples = integer(n),
   rms_x = numeric(n),
   rms_y = numeric(n),
   stringsAsFactors = FALSE
)

for (i in seq_along(trial_list)) {
   x = trial_list[[i]]$copX
   y = trial_list[[i]]$copY
   x = x - mean(x, na.rm = TRUE)
   y = y - mean(y, na.rm = TRUE)
   rms_df$n_samples[i] = length(x)
   rms_df$rms_x[i] = sqrt(mean(x^2, na.rm = TRUE))
   rms_df$rms_y[i] = sqrt(mean(y^2, na.rm = TRUE))
}

rms_df
plot(rms_df$rms_x, rms_df$rms_y, xlab = "RMS X", ylab = "RMS Y")
cor(rms_df$rms_x, rms_df$rms_y)

cor.test(rms_df$rms_x, rms_df$rms_y)
