#plot colors from table 
###Clear workspace
rm (list=ls())
#read munsell RGB match table and soil colors
mnsl.tbl = read.csv(file="./Munsell_RGB.csv")
col.tbl2 = read.csv(file="./Data/color.csv")

col.tbl = col.tbl2[which(col.tbl2$Moisture=="Moist"),]

#Maps a Munsell colour to RGB (0,1)
m.rgb <- function(col){
  positions <- match(col, mnsl.tbl$Munsell)
  R.val = ifelse(mnsl.tbl[positions, "R"]==0,0,(mnsl.tbl[positions, "R"]/255))
  G.val = ifelse(mnsl.tbl[positions, "G"]==0,0,(mnsl.tbl[positions, "G"]/255))
  B.val = ifelse(mnsl.tbl[positions, "B"]==0,0,(mnsl.tbl[positions, "B"]/255))
  return(c(R.val, G.val, B.val))
}

sols = dim(col.tbl2)[1]/2
temps = dim(col.tbl)[2]-2

high = sols*200
wide = temps*100

## rect(xleft, ybottom, xright, ytop)
pdf("colors2.pdf", width = 12, height = 8)
par(mfrow=c(1,2), oma = c(5,4,0,0)+0.1, mar = c(0,0,1,1)+0.1)

plot(c(0, wide), c(0, high), type= "n", xlab=NA, ylab=NA, axes=FALSE, main = "Dry Color")
axis(side = 1, at=c(25, 150, 250, 350, 450, 550, 650),)
mtext(side = 1, expression(paste("Heating temperature (", degree, "C)")), line = 2)
y2 = high
col.tbl = col.tbl2[which(col.tbl2$Moisture=="Dry"),]
mmoist = as.character(col.tbl[1,2])
for (i in 1:sols){ #soils 
  msoil = as.character(col.tbl[i,1])
  text(-25, y2, msoil, pos = 4)
  y2 = y2-60
  y1 = y2-100
  x1 = 10
  x2 = 100
  for (k in 3:9){ #temp
    mcol = as.character(col.tbl[i,k])
    rgbcol = m.rgb(mcol)
    col.x = (x1+x2)/2
    col.y = y2
    text(col.x, col.y, mcol, pos = 3, cex = 0.75)
    rect(x1, y1, x2, y2, col=rgb(rgbcol[1],rgbcol[2],rgbcol[3]), border="black")
    x1 = x2+10
    x2 = x1+90
  }
  y2 = y1-50
  y1 = y2-100
}
plot(c(0, wide), c(0, high), type= "n", xlab=NA, ylab=NA, axes=FALSE, main = "Moist Color")
axis(side = 1, at=c(25, 150, 250, 350, 450, 550, 650),)
mtext(side = 1, expression(paste("Heating temperature (", degree, "C)")), line = 2)
y2 = high
col.tbl = col.tbl2[which(col.tbl2$Moisture=="Moist"),]
mmoist = as.character(col.tbl[1,2])
for (i in 1:sols){ #soils 
  msoil = as.character(col.tbl[i,1])
  text(-25, y2, msoil, pos = 4)
  y2 = y2-60
  y1 = y2-100
  x1 = 10
  x2 = 100
  for (k in 3:9){ #temp
    mcol = as.character(col.tbl[i,k])
    rgbcol = m.rgb(mcol)
    col.x = (x1+x2)/2
    col.y = y2
    text(col.x, col.y, mcol, pos = 3, cex = 0.75)
    rect(x1, y1, x2, y2, col=rgb(rgbcol[1],rgbcol[2],rgbcol[3]), border="black")
    x1 = x2+10
    x2 = x1+90
  }
  y2 = y1-50
  y1 = y2-100
}
dev.off()