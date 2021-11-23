# Challenge 1
Hi My name is Hayward Wong and this is my repository for challenge 1

## Summary
This week’s challenge is an individual coding challenge on the theme of data wrangling. I made code lines from ```DS_starter.R``` more efficient using ```dplyr``` package and ```ggplot2```to create my own version of it named ```Curlew.R```, which looked into trends of observed Eurasian Curlew population in the U.K. from the  Living Planet Index (LPI).
In addition to ```Curlew.R``` which is an improved version of ```DS_starter.R```, I also created ```Goose.R``` which looked into Goose populations in the U.K. While wrangling the data, I realised there was only 1 Curlew observation id in the UK that is longer than 15 years, and I think it is a wasted oppurtunity to plot only one site on the map. Therefore I decided to add more species and sites, this time working on 4 goose species from two different genus.
It is also important to note that Stavros is providing feedback for this challenge, Thankyou Stavros.


### Important Files

| File name | Description |
| ---- | ---- |
```README.md``` | Basic info on the repository
```challenge_1_instructions.md``` | Instruction of this challenge
```Raw_data/LPI_birds.csv``` | csv file containing observation of bird data from LPI
```Raw_data/site_coords.csv``` | csv file containing coordinates of observations for LPI_birds.csv
```DS_starter.R``` | original R script provided that needed to be made more efficient
```Eurasian_Curlew/Curlew.R```|a more concise version of ```DS_starter.R```, uses ```LPI_birds.csv```&```site_coords.csv``` to visualise curlew population in the UK
```Eurasian_Curlew/Curlew_Figures/UK_Curlew_trends.pdf```|pdf file of chart visualising curlew population in the U.K. as a trend line
```Eurasian_Curlew/Curlew_Figures/UK_Curlew_site.pdf```|pdf file of map showing location of curlew observations
```Eurasian_Curlew/Curlew_Figures/UK_Curlew_Population.pdf```|pdf file combining the curlew map and curlew chart
```Goose/Goose.R```|uses ```LPI_birds.csv```&```site_coords.csv``` to visualise goose populations in the UK
```Goose/Goose_Figures/UK_Goose_trends.pdf```|pdf file of chart visualising goose populations in the U.K. as a trend line
```Goose/Goose_Figures/UK_Goose_site.pdf```|pdf file of map showing location of goose observations
```Goose/Goose_Figures/UK_Goose_Population.pdf```|pdf file combining the goose map and goose chart
