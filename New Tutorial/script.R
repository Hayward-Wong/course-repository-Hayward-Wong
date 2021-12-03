install.packages("rgl")

library("tidyverse")
library("rgl")
library("plot3D")

catchwater <- read.csv("data/Catchment_data_1967-1970.csv")


str(catchwater)

x <- rain <- catchwater$Rain..mm.day.
y <- disc. <- catchwater$Discharge..mm.day.
z <- evap. <- catchwater$Evap..mm.day.

scatter3D(x, y, z, clab = c("Evaporation", "mm/day"))


scatter3D(x, y, z, colvar = NULL, col = "blue",
          pch = 19, cex = 0.5)

scatter3D(x, y, z, bty = "b2", pch = 1, 
          col.var = as.integer(catchwater$Year), 
          col = c("#FF5733","#41AC0E","#5416E0","#6D3F0E"),
          pch = 1, ticktype = "detailed",
          colkey = list(at = c(2, 3, 4, 5), side = 1, 
                        addlines = TRUE, length = 0.5, width = 0.5,
                        labels = "1967","1968","1969","1970") )


scatter3D(x, y, z,bty = "b2", pch = 1,  
          main = "catchment_data", xlab = "Rain..mm.day.",
          ylab ="Discharge..mm.day.", zlab = "Evap..mm.day.")

plot3d(x,y,z)

rgl.snapshot("figures/initial.png")

rgl.postscript("plot_1.pdf",fmt = "pdf", drawText = TRUE)


catchment_data <- catchwater %>% 
  mutate(year_colour = case_when(Year == "1967" ~ "red",
                                 Year == "1968" ~ "grey",
                                 Year == "1969" ~ "blue",
                                 Year == "1970" ~ "green",
                                 TRUE ~ NA_character_))



plot3d(x,y,z, 
       col = catchment_data$year_colour)

rgl.snapshot("figures/coloured.png")

rgl.close()

plot3d(x,y,z,
       col = catchment_data$year_colour,
       type = 's', 
       radius = 5)
rgl.snapshot("figures/spheres.png")
rgl.close()



plot3d(x,y,z,
       col = catchment_data$year_colour,
       type = 's', 
       radius = 5,
       xlab ="Rainfall mm/day", 
       ylab = "Discharge mm/day", 
       zlab = "Evaportation mm/day")


rgl.snapshot("figures/labels.png")
rgl.close()

plot3d(x,y,z,
       col = catchment_data$year_colour,
       type = 's', 
       radius = 5,
       xlab ="Rainfall mm/day", 
       ylab = "Discharge mm/day", 
       zlab = "Evaportation mm/day")

legend3d("topright", legend = c("1967","1968","1969","1970"), 
         pch = 16, 
         col = c("red","grey","blue","green"), 
         inset = c(0.05))

rgl.snapshot("figures/legend.png")

rgl.viewpoint(theta = 30, phi = 30)

rgl.snapshot("figures/viewpoint.png")


plot3d(x,y,z,
       col = catchment_data$year_colour,
       type = 's', 
       radius = 5,
       xlab ="Rainfall mm/day", 
       ylab = "Discharge mm/day", 
       zlab = "Evaportation mm/day")

rgl.viewpoint(theta = 30, phi = 30, zoom = 0.5)

rgl.snapshot("figures/zoomin.png")


plot3d(x,y,z,
       col = catchment_data$year_colour,
       type = 's', 
       radius = 5,
       xlab ="Rainfall mm/day", 
       ylab = "Discharge mm/day", 
       zlab = "Evaportation mm/day")

rgl.viewpoint(theta = 30, phi = 30, zoom = 1.5)

rgl.snapshot("figures/zoomout.png")


htmlwidgets::saveWidget(rglwidget(), 
                        file = "3dscatter.html",
                        selfcontained = FALSE)

movie3d(spin3d(axis = c(0, 0, 1)), duration = 3,
        dir = getwd())


 
