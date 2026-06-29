install.packages("readx1")

library(readxl)
library(dplyr)
library(zoo)

df <- read_excel(
   path = "p3t01_022626_214pm.xlsx",
   sheet = 1,           # or sheet name: "Sheet1"
   skip = 0,            # rows to skip at top
   col_names = F
) %>% as.data.frame()


tail(df)

colnames(df) = c( "TimeOpen","Frc1","Frc2","Frc3","Frc4","copX","copY","FrcTot")


df[,1] = df[,1] - df[1,1]


7719472/60000
4453660/60000


df$xRoll = rollmean(df$copX, k= 20, fill = mean(df$copX[1:19]),na.pad = T)

df$xDif = c(0,df$copX[2:nrow(df)]-df$copX[1:(nrow(df)-1)])

nrow(df[abs(df$xDif)>1,])

df[29110:29114,]

df[25623:25628,]

df[39920:39925,]

df$TimeDif = c(0,df$TimeOpen[2:nrow(df)]-df$TimeOpen[1:(nrow(df)-1)])

nrow(df[abs(df$TimeDif)>40,])
df[abs(df$TimeDif)>40,]

plot(df$xRoll)

plot(df$xRoll[39850:40000])
plot(df$copX[39850:40000])

abline(v=900)

plot(df$xRoll[39850:40000])

abline(v=900)




df2 <- read_excel(
   path = "p3t02_022626_237pm.xlsx",
   sheet = 1,           # or sheet name: "Sheet1"
   skip = 0,            # rows to skip at top
   col_names = F
) %>% as.data.frame()

colnames(df2) = c( "TimeOpen","Frc1","Frc2","Frc3","Frc4","copX","copY","FrcTot")

tail(df2)

df2$TimeDif = c(0,df2$TimeOpen[2:nrow(df2)]-df2$TimeOpen[1:(nrow(df2)-1)])
nrow(df2[abs(df2$TimeDif)>40,])
df2[abs(df2$TimeDif)>40,]

11024999 /60000
6519437/60000
4445731 /60000



head(df[,1:8])==head(df2[,1:8])

tail(df[,1:8])==tail(df2[,1:8])
