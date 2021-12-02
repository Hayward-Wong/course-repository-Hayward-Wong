# challenge4-instructions
_Instructions for Challenge 4 - Spatial analysis using the Google Earth Engine_

## Has forest cover change occurred across protected areas?
## Data Science Challenge
### _25th November 2021_

### Google Earth Engine link
https://code.earthengine.google.com/

If you need a login to access the script you can use the following:
- Login: datascienceees@gmail.com
- Password: DataScienceEES2019!

Login to find the challenge script for your group.

Link to the GEE scripts: 
- Group 1: https://code.earthengine.google.com/ac3ddea0c65a560931f17bd9bb178136
- Group 2: https://code.earthengine.google.com/d34419a235b1e84f802c7007210ba170
- Group 3: https://code.earthengine.google.com/85d27e7b0a4c3d381ea14d45ac098cba
- Group 4: https://code.earthengine.google.com/3f911d57652eb8ae74af9fb5f4a4c34d
- Group 5: https://code.earthengine.google.com/8e3b174404344696b6a5c1d1c2625a22

Feedback:
- Group 1: https://docs.google.com/document/d/1js8sHmK0ifvJRKY1FyO9UPaF0HyccIbNlsa0fbOBncw/edit?usp=sharing 
- Group 2: https://docs.google.com/document/d/1gdD83hr9QQ93qPh88P-yVRhSb_2lSAeldJc-Kml8s6U/edit?usp=sharing 
- Group 3: https://docs.google.com/document/d/1-5zVwbjL49G_42L8C4HINZk9MO3q4j1QBGuXTf80hKI/edit?usp=sharing 
- Group 4: https://docs.google.com/document/d/1Y6QSo42c1shOLwfbXU1cJZe3rPJAkE8KVme_x_BOHM8/edit?usp=sharing 
- Group 5: https://docs.google.com/document/d/1NAFX9y0Uv7stvtlSAlVSjqhkdeqsCsRknFuyLK9Ecmg/edit?usp=sharing 


# Overarching goal
_*Investigate forest cover change across protected areas.*_


# Challenge aims
- Map forest cover change from the Hansen et al. Database for your group's protected area as well for the larger region surrounding the protected area
- Create a figure in R visualising the amounts of forest cover change over time
- Report the state of forests in your group's protected area in a Markdown document


# Challenge marking criteria
- Challenge: The creation of a clear workflow with even division of tasks among team members and evidence of effective group work and collaborative coding. The use of JavaScript code in the Earth Engine code editor to conduct the analysis and mapping and R code to produce the data visualisations. - 25%

- Creativity: The development of clear questions and predictions and an informative summary of results. The effective and creative presentation of results, figures, maps, any statistical analyses. - 25%

- Reproducibility: Appropriate code and comments, clear interpretation of findings and professional presentation of your markdown document and repository. - 25%

- Participation: Evidence of group work, leadership, project management, use of GitHub repository to carry out your work as a team. Indivudal contributions of all group members contributing to the final product. - 25%



_Use this document as a template to fill in as you progress through the challenge._

# Title

Forest cover change in the High Uintas Wilderness Area, Utah


# Group name

Group 3 - Uintas!


# Group participants

- Ailsa Staderlee
- Zuzi Koscikova
- Filoteea Moldovan
- Hayward Wong
- Nina Rae Shortland


# Research question
__How much forest cover loss and gain has occurred in your protected area?__


# Introduction

The High Uintas Wilderness Area is a protected area located in Utah, USA, jointly managed by the Ashley and Wasatch-Cache National Forests.  The park is 460,000 acres and home to an expanse of forests, thousands of natural lakes and the highest mountains in Utah with elevations up to 13,258 feet (Utah, 2021).  Protected areas are important designated areas for land and biodiversity conservation, and effective management plans are essential for implementing.

We aim to assess how forest cover has changed within the Uintas protected area.  It is our hypothesis that although there is a management plan for the park (USDA, 2003) timber activities such as clearcutting outweigh conservation activities for the forests so overall forest change will be a loss over time.


# Workflow (see also in projects section)

**General Organisation**
- Filo: keep in touch with Hayward, organise repo, talk about job divison
- Zuzi: share screen to explore code/data
- Aisla: check feedback, google questions
- Nina: time management
- Hayward: Making the java code work on GEE and making the map.

**Workflow**
*Together*
- Explore Java code and run it on EGG
- Explore .csv file
-  Read instructions

*Aisla*
- Managed to added code to the gitignore file so we can't puch Rproject

*Hayward (because it's easily to work individually*
- Work on creating figure structures

*Do some data wrangling*
- Zuzi and Filo

*Do some research to think of hypotheses*
- Nina and Aisla

*Think of stats*
- Nina and Aisla

*Think of how to display data*

*Code for data visualisation*
- Zuzi

*Editing README*
- Nina

*Run codes on GEE and make the map*
- Hayward




# Specific hypotheses and predictions

The park is home to many terrestrial and aquatic habitats, mountains and forests.  As well as roads and human recreational activities such as hiking and camping.  Livestock grazing is also allowed in the park (Carter et al., 2020) and there is timber management such as clearcutting (USDA, 2021).  The conservation strategy is focused mainly on wildlife not forest restoration therefore we pridict the timber activities having a detrimental effect on forest cover overall leading to declines over time.  In this area forest fire occur and in the current climate are increasing which could have a big impact on forest cover.


# Methods

In Google Earth Engine we used the dataset ```Global Forest Change 2000–2017``` from ```Hansen/UMD/Google/USGS/NASA```, which is free to use and build upon the material for any purpose as long as credit is given.  Using JavaScript code in the Earth Engine code editor we conducted analysis and mapped the forest cover change in the area. We then took screenshot of the map and uploaded it to the markdown.  We extracted the data for forest cover, gain and loss for the Uintas area that we were looking at and exported the data to R studio, where we manipulated the data before visualisation.


# Data vis and summary methods

Once we had reformatted the data we used ggplot to plot a scatter plot of the changes in forest cover over time.

```
(Scatterplot <- ggplot(Final_data1, aes(years, difference)) +
geom_point(aes(color = difference)) +
scale_color_viridis(option = "D")+
theme_bw() +
stat_smooth(method = "lm") +
labs(title = "Changes in forest cover from 2001-2016",
x = "\n Years", y = "Overall changes in forest cover\n"))

ggsave("Outputs/Scatterplot.png")

```


# 1. Maps of forest cover change for your protected area

_*Describe your results using appropriate scientific writing. Include maps of your protected area with informative captions.*_

_*Your maps*_

The map below is the map of forest cover change for High Uintas Wilderness protected area. 

![HU_forest_change_map](https://user-images.githubusercontent.com/91271258/143480825-cb246ba7-c05b-43c7-8ff7-91b136f147ab.jpg)

The park area is shown in red, and the tree cover is shown in various level of green.
Forest cover loss will be shown in purple and forest cover gain in orange.

# 2. Visualisation of the amount of forest cover loss and gain for your protected area

![image](https://user-images.githubusercontent.com/55798292/143479027-6032967b-5f09-454d-9b6d-8d587721ec2a.png)

Figure 1. Change in forest cover from 2001 to 2005 in Uintas park.


# 3. How do your results compare with your predictions? What do you think might explain the patterns you found?

_*Use your scientific and critical thinking skills to provide a conclusion summary statement of how land cover is changing across the protected area that you studied.*_

__*Your text and/or figures.*__


# 4. What other datasets, available within the GEE, could you use to test the potential drivers of forest cover change in your group's protected area that you identified in point #3. ?

One potential dataset would be the Global PALSAR-2/PALSAR Forest/Non-Forest Map:
https://developers.google.com/earth-engine/datasets/catalog/JAXA_ALOS_PALSAR_YEARLY_FNF
this dataset maps forest or non-forested areas by backscatter. It has a resolution of 25 metres However, this dataset struggles at high latitude. As the park is at around 40 degrees north, the dataset would be useable.

Alternatively, using USFS Landscape Change Monitoring System v2020.5:
https://developers.google.com/earth-engine/datasets/catalog/USFS_GTAC_LCMS_v2020-5
This dataset shows modelled change in land use for each year which would allow us to monitor the amount of forest cover in High Uintas park over time while also showing what land us is replacing it.

Additionally, as fires may be a driver of landuse change, a fire monitoring dataset could be used like FireCCI51: MODIS Fire_cci Burned Area Pixel Product, Version 5.1:
https://developers.google.com/earth-engine/datasets/catalog/ESA_CCI_FireCCI_5_1


# 5. What research question and hypotheses would you test with those additional datasets in your proposed future research and why does that research matter for the park management?

The Global PALSAR-2/PALSAR Forest/Non-Forest Map dataset maps forest or non-forested areas by backscatter, which could answer questions such as "Does non-forested area in High Uintas increase overtime?", which would be important for management as High Uintas is a protected area and its important to keep forested area.

The USFS Landscape Change Monitoring System v2020.5, it modeled change in land use for each year, An interesting research question would be "How does Landuse change in High Uintas over time?" Knowing the accurate landuse and understand it changes is very important when managing a protected area as it allow for better conservation plannings.

Fire monitoring dataset could be answer potential research questions such as "How fire affect land use change in your protected area?" since as mentioned in Q4, fires may be a driver of landuse change and should be carefully monitored

# Conclusions

We found that forest loss has exceeded forest gain, therefore overall forest cover in the Uintas protected area has decreased.  Also that forest loss is occuring year on year whereas gain is only occuring in certain years. This has implications on the protected area as if the loss continues year on year the forest cover will could be forecast to continue to decrease.  This will remove primary productivity from the area along with wildlife habitat, and increase chances of erosion on slopes affecting the water cycle.  An effective conservation and tree planting strategy needs to be implemeneted in the area to counteract and timber felling carried out.



# References

Carter, J. , Vasquez, E. and Jones, A. (2020) Spatial Analysis of Livestock Grazing and Forest Service Management in the High Uintas Wilderness, Utah. Journal of Geographic Information System, 12, 45-69. doi: 10.4236/jgis.2020.122003.

Google Earth Engine (2021) https://earthengine.google.com/

Hansen Global Forest Change v1.5 (2000-2017): Hansen, M. C., P. V. Potapov, R. Moore, M. Hancher, S. A. Turubanova, A. Tyukavina, D. Thau, S. V. Stehman, S. J. Goetz, T. R. Loveland, A. Kommareddy, A. Egorov, L. Chini, C. O. Justice, and J. R. G. Townshend. (2013) “High-Resolution Global Maps of 21st-Century Forest Cover Change.” Science 342 (15 November): 850–53. Data available on-line at: https://earthenginepartners.appspot.com/science-2013-global-forest.

Utah.com (2021) https://utah.com/uinta-mountains

USDA (2013) Land and Resource Management Plan. https://www.fs.usda.gov/Internet/FSE_DOCUMENTS/stelprdb5444983.pdf


