library(tidyverse)
library(maps)
library(ggthemr)
library(ggthemes)
library(plotly)
library(fmsb)
library(ggstream)

# Establish theme 
ggthemr('light',type="outer",layout="minimal")
my_pal <- define_palette(
  swatch = c('yellow', 'salmon', 'forestgreen', 'dodgerblue', 'brown',
             'pink', 'lightgrey',"darkorchid3","orange","honeydew4","violet",
             "indianred1","cyan4","darkgoldenrod3","lightblue3",
             "aquamarine1","coral2","cornsilk3","burlywood2","deepskyblue3",
             "firebrick2","gold1","deeppink3","lawngreen","lightcyan2",
             "hotpink4","maroon","olivedrab","plum2","slategray2","tan1",
             "cornsilk","azure1","blanchedalmond","darkolivegreen2",
             "darkseagreen2","mediumpurple1","salmon1","turquoise1",
             "slateblue2"), 
  gradient = c(lower = 'indianred1', upper = 'deepskyblue3')
)
ggthemr(my_pal)


# Set working directory
setwd("/Users/annienguyen/STA404/Final/Final-grad")

# Download stroke data from 
# https://www.kaggle.com/datasets/fedesoriano/stroke-prediction-dataset
stroke <- read.csv("healthcare-dataset-stroke-data.csv")

# Download bodyfat data from 
# https://www.kaggle.com/datasets/fedesoriano/body-fat-prediction-dataset
bodyfat_data <- read.csv("bodyfat.csv")

# Download cancer data from 
# https://www.kaggle.com/datasets/utkarshxy/who-worldhealth-statistics-2020-complete?select=30-70cancerChdEtc.csv
cancer_data <- read.csv("30-70cancerChdEtc.csv")

# Download suicide data from 
# https://www.kaggle.com/datasets/russellyates88/suicide-rates-overview-1985-to-2016
suicide <- read.csv("master.csv")

# Download life-exp data from 
# https://www.kaggle.com/datasets/kumarajarshi/life-expectancy-who
life_exp <- read.csv("LifeExpectancyData.csv")


# How likely are you to have strokes
  stroke <- stroke %>% 
    mutate(age = as.integer(age),
           class = "N/A",
           smoking_status = str_to_title(smoking_status))
  
  for (i in 1:nrow(stroke))
  {
    if (stroke$age[i]<=1)
      stroke$class[i] = "Infants"
    
    if (stroke$age[i] <10 & stroke$age[i] >1)
      stroke$class[i] = "Children"
    
    if (stroke$age[i] <20 & stroke$age[i] >10 | stroke$age[i] ==10)
      stroke$class[i] = "Teenagers"
    
    if (stroke$age[i] <65 & stroke$age[i] >20 | stroke$age[i] ==20)
      stroke$class[i] = "Adults"
    
    if (stroke$age[i] <120 & stroke$age[i] >65 | stroke$age[i] ==65)
      stroke$class[i] = "Elders/Seniors"
    
    
    if (stroke$avg_glucose_level[i] > 55 & stroke$avg_glucose_level[i] <= 100)
      stroke$glucose_level[i] = "55 - 100"
    
    if (stroke$avg_glucose_level[i] > 100 & stroke$avg_glucose_level[i] <= 150)
      stroke$glucose_level[i] = "Over 100 - 150"
    
    if (stroke$avg_glucose_level[i] > 150 & stroke$avg_glucose_level[i] <= 200)
      stroke$glucose_level[i] = "Over 150 - 200"
    
    if (stroke$avg_glucose_level[i] > 200)
      stroke$glucose_level[i] = "Over 200"
  }
  
  stroke <- stroke[-3117,] # Remove "Other" gender - 1 value only

  test_stroke<- stroke %>% 
    filter(gender =="Female",
           class == "Adults",
           hypertension == 0,
           ever_married < "Yes",
           work_type == "Private",
           Residence_type == "Urban",
           glucose_level ==  "Over 100 - 150",
           smoking_status == "Never Smoked") %>% 
    arrange(stroke) %>%
    count(stroke) %>% 
    mutate(label = "N/A")
 
    for (i in 1:nrow(test_stroke))
    {
      if (test_stroke$stroke[i] == 0)
        test_stroke$label[i] = "No stroke"
      if (test_stroke$stroke[i] == 1)
        test_stroke$label[i] = "Stroke"
    }
  # Basic piechart
  stroke_pie <- plot_ly(test_stroke, labels = ~label, values = ~n, type = 'pie',
                        textposition = 'inside',
                        textinfo = 'label+percent',
                        insidetextfont = list(color = '#FFFFFF'),
                        hoverinfo = 'text',
                        text = ~paste(label),
                        marker = list(colors = colors,
                                      line = list(color = '#FFFFFF', 
                                                  width = 1)),
                        showlegend = FALSE)
  

  stroke_pie <- stroke_pie %>% 
    layout(title = 'Liability of you having a stroke',
                        xaxis = list(showgrid = FALSE, zeroline = FALSE, 
                                     showticklabels = FALSE),
                        yaxis = list(showgrid = FALSE, zeroline = FALSE, 
                                     showticklabels = FALSE))

  stroke_pie

# Predicting your bodyfat 
  bodyfat_data <- bodyfat_data %>% 
    select(-Density) %>% 
    select(-Neck, - Knee, - Ankle, -Forearm, - Biceps, - Wrist)
  
  
  bodyfat_data <- bodyfat_data[-42,] # Remove an extreme height value
  bodyfat_data <- bodyfat_data[-39,] # Remove an extreme abdomen circ value

  
  bodyfat <- bodyfat_data %>% 
    mutate(age_group = "N/A", weight_class = "N/A", height_class = "N/A",
           chest_group = "N/A", abdomen_group = "N/A",hip_group = "N/A", 
           thigh_group = "N/A")

  
  for (i in 1:nrow(bodyfat))
  {
    # Create age groups
    if (bodyfat$Age[i] <35 & bodyfat$Age[i] >=20)
      bodyfat$age_group[i] = "20 - Under 35 years old"
    
    if (bodyfat$Age[i] <50 & bodyfat$Age[i] >=35)
      bodyfat$age_group[i] = "35 - Under 50 years old"
    
    if (bodyfat$Age[i] <65 & bodyfat$Age[i] >=50)
      bodyfat$age_group[i] = "50 - Under 65 years old"
    
    if (bodyfat$Age[i] <120 & bodyfat$Age[i] >= 65)
      bodyfat$age_group[i] = "65 and older"
    
    
    # Create weight groups
    if (bodyfat$Weight[i] <165 & bodyfat$Weight[i] >=115)
      bodyfat$weight_class[i] = "115 - Under 165 lbs"
    
    if (bodyfat$Weight[i] <215 & bodyfat$Weight[i] >=165)
      bodyfat$weight_class[i] = "165 - Under 215 lbs"
    
    if (bodyfat$Weight[i] <265 & bodyfat$Weight[i] >=215)
      bodyfat$weight_class[i] = "215 - Under 265 lbs"
    
    if (bodyfat$Weight[i] <315 & bodyfat$Weight[i] >= 265)
      bodyfat$weight_class[i] = "265 - Under 315 lbs"
    
    if (bodyfat$Weight[i] <365 & bodyfat$Weight[i] >= 315)
      bodyfat$weight_class[i] = "315 - Under 365 lbs"
    
    
    # Create height groups
    if (bodyfat$Height[i] <68 & bodyfat$Height[i] >=64)
      bodyfat$height_class[i] = "64 - Under 68 inches"
    
    if (bodyfat$Height[i] <72 & bodyfat$Height[i] >=68)
      bodyfat$height_class[i] = "68 - Under 72 inches"
    
    if (bodyfat$Height[i] <76 & bodyfat$Height[i] >=72)
      bodyfat$height_class[i] = "72 - Under 76 inches"
    
    if (bodyfat$Height[i] <80 & bodyfat$Height[i] >= 76)
      bodyfat$height_class[i] = "76 - Under 80 inches"


    
    # Create chest circ group
    if (bodyfat$Chest[i] <98 & bodyfat$Chest[i] >=79)
      bodyfat$chest_group[i] = "79 - Under 98 cm"
    
    if (bodyfat$Chest[i] < 117 & bodyfat$Chest[i] >=98)
      bodyfat$chest_group[i] = "98 - Under 117 cm"

    if (bodyfat$Chest[i] <= 137 & bodyfat$Chest[i] >=117)
      bodyfat$chest_group[i] = "117 - 137 cm"
    
    
    # Create abdomen circ group
    if (bodyfat$Abdomen[i] <84 & bodyfat$Abdomen[i] >=69)
      bodyfat$abdomen_group[i] = "69 - Under 84 cm"
    
    if (bodyfat$Abdomen[i] < 98 & bodyfat$Abdomen[i] >=84)
      bodyfat$abdomen_group[i] = "84 - Under 98 cm"
    
    if (bodyfat$Abdomen[i] < 113 & bodyfat$Abdomen[i] >=98)
      bodyfat$abdomen_group[i] = "98 - Under 113 cm"
    
    if (bodyfat$Abdomen[i] <= 127 & bodyfat$Abdomen[i] >=113)
      bodyfat$abdomen_group[i] = "113 - 127 cm"
    
    
    # Create hip circ group
    if (bodyfat$Hip[i] <105 & bodyfat$Hip[i] >=85)
      bodyfat$hip_group[i] = "85 - Under 100 cm"
    
    if (bodyfat$Hip[i] <= 127 & bodyfat$Hip[i] >=105)
      bodyfat$hip_group[i] = "105 - 127 cm"
    
    
    # Create thigh circ group
    if (bodyfat$Thigh[i] <57 & bodyfat$Thigh[i] >=47)
      bodyfat$thigh_group[i] = "47 - Under 57 cm"
    
    if (bodyfat$Thigh[i] <= 67 & bodyfat$Thigh[i] >=57)
      bodyfat$thigh_group[i] = "57 - Under 67 cm"
    
    if (bodyfat$Thigh[i] <= 87 & bodyfat$Thigh[i] >=67)
      bodyfat$thigh_group[i] = "67 - 87 cm"
    
    

  }
  
  # Filtering information 
  test_bodyfat <- bodyfat %>% 
    filter(age_group == "20 - Under 35 years old",
           weight_class == "115 - Under 165 lbs",
           height_class == "64 - Under 68 inches",
           chest_group == "79 - Under 98 cm",
           abdomen_group == "69 - Under 84 cm",
           hip_group == "85 - Under 100 cm",
           thigh_group == "47 - Under 57 cm"
    )%>% 
    arrange(BodyFat) %>%
    mutate(Approximation = mean(BodyFat)) 
  
  body_max <- data.frame(matrix(ncol=8, nrow = 1))
  colnames(body_max) <- colnames(bodyfat_data)
  rownames(body_max) <- "You"
  
  # To use the fmsb package, I have to add 2 lines to the dataframe: the max
  # and min of each variable to show on the plot!
  body_max <- rbind(rep(20,ncol(body_max)),rep(0,ncol(body_max)),body_max)
  body_max[1,] <- c(47.5, 81,262.75,77.75,43.9,128.3,126.2,
                    125.6)

  body_max["You",1] <- test_bodyfat$Approximation[1]
  
  body_max["You",2:8] <- c(20,20,37,250,56,43.9,120)
  
  body_max["You_percent",] <- c(0)

  
  for (i in 1:ncol(body_max))
  {
    body_max["You_percent",i] = (body_max["You",i]/body_max[1,i] *100)
  }

  r_val <- unlist(body_max["You_percent",], use.names = FALSE)
  text_val <- unlist(body_max["You",], use.names = FALSE)

  body_info <- plot_ly(body_max,
    type = 'scatterpolar', r = r_val, theta = colnames(body_max),
    fill = 'toself', hoverinfo = 'text',text = text_val, name = "You"
  ) 
  body_info <- body_info %>%
    layout(
      polar = list(
        radialaxis = list(
          visible = F,
          range = c(0,100)
        )
      ),
      showlegend = F
    )
  
  body_info
  
  
  
# How likely are you to die from cancer 
  cancer <- cancer_data %>% 
    mutate(Sex = Dim1,
           Liability = First.Tooltip) %>% 
    select(-Dim1, - First.Tooltip)
  
  names(cancer)[names(cancer) == 'Location'] <- 'Country'
  
  
  for (i in 1:nrow(cancer))
  {
    if (cancer$Country[i] == "United States of America")
      cancer$Country[i] = "USA"
    
    if (cancer$Country[i] == "Russian Federation")
      cancer$Country[i] = "Russia"
    
    if (cancer$Country[i] == "Bolivia (Plurinational State of)")
      cancer$Country[i] = "Bolivia"
    
    if (cancer$Country[i] == "Venezuela (Bolivarian Republic of)")
      cancer$Country[i] = "Venezuela"
    
    if (cancer$Country[i] == "Viet Nam")
      cancer$Country[i] = "Vietnam"
    
    if (cancer$Country[i] == "Lao People's Democratic Republic")
      cancer$Country[i] = "Laos"
    
    if (cancer$Country[i] == "Samoa")
      cancer$Country[i] = "American Samoa"
    
    if (cancer$Country[i] == "Brunei")
      cancer$Country[i] = "Brunei Darussalam"
    
    if (cancer$Country[i] == "Côte d’Ivoire")
      cancer$Country[i] = "Ivory Coast"
    
    if (cancer$Country[i] == "Congo")
      cancer$Country[i] = "Republic of Congo"
    
    if (cancer$Country[i] == "Czechia")
      cancer$Country[i] = "Czech Republic"
    
    if (cancer$Country[i] == "Dominican Republic")
      cancer$Country[i] = "Dominica"
    
    if (cancer$Country[i] == "Micronesia (Federated States of)")
      cancer$Country[i] = "Micronesia"
    
    if (cancer$Country[i] == "United Kingdom of Great Britain and 
                                Northern Ireland")
      cancer$Country[i] = "UK"
    
    if (cancer$Country[i] == "Iran (Islamic Republic of)")
      cancer$Country[i] = "Iran"
    
    if (cancer$Country[i] == "Republic of Korea")
      cancer$Country[i] = "South Korea"
    
    if (cancer$Country[i] == "Republic of Moldova")
      cancer$Country[i] = "Moldova"
    
    if (cancer$Country[i] == "The former Yugoslav Republic of Macedonia")
      cancer$Country[i] = "North Macedonia"
    
    if (cancer$Country[i] == "Democratic People's Republic of Korea")
      cancer$Country[i] = "North Korea"
    
    if (cancer$Country[i] == "Georgia")
      cancer$Country[i] = "South Georgia"
    
    if (cancer$Country[i] == "Syrian Arab Republic")
      cancer$Country[i] = "Syrian"
    
    if (cancer$Country[i] == "Georgia")
      cancer$Country[i] = "South Georgia"
  }
  
  world_map_data <- map_data("world")
  
  for (i in 1:nrow(world_map_data))
  {
    if (world_map_data$region[i] == "Antigua" | 
        world_map_data$region[i] == "Barbuda")
      world_map_data$region[i] = "Antigua and Barbuda" 
    
    if (world_map_data$region[i] == "Trinidad" | 
        world_map_data$region[i] == "Tobago")
      world_map_data$region[i] = "Trinidad and Tobago"
    
    if (world_map_data$region[i] == "Grenadines" | 
        world_map_data$region[i] == "Saint Vincent")
      world_map_data$region[i] = "Saint Vincent and the Grenadines"
  }
  

  cancer_map <- cancer %>% 
    filter(Period == "2016") %>% 
    right_join(world_map_data, by = c("Country"="region")) %>% 
    select(-subregion) 
  
  cancer_map_female <- cancer_map %>%  # Input choice : gender
    filter(Sex=="Female")

  map_plot <- ggplot() +
    geom_polygon(aes(x=long,y=lat,group=group, fill=Liability, subgroup = Country),
                 data=cancer_map_female)+ 
    theme_map()+
    coord_quickmap()+
    labs(title="Cancer liability for each country (%)") +
    theme(plot.title = element_text(hjust=0.5),
          plot.subtitle = element_text(hjust=0.5),
          legend.position = "none") +
    scale_fill_gradient(name="Cancer liability (%)",low="dodgerblue",
                        high="salmon")
  
  ggplotly(map_plot, tooltip = c("fill","subgroup"))

  
  
# Suicide by year and country
  suicide <- suicide %>% 
    mutate(sex = str_to_title(sex))
    
  # Filter by country, year
  suicide_selected <- suicide %>% 
    select(-country.year, -HDI.for.year) %>% 
    filter(country == "El Salvador",
           year > 1992 & year < 2008, sex == "Female") %>%
    group_by(year, age)
    
  stream <- streamgraph(suicide_selected,"age", "suicides.100k.pop", "year") %>%
    sg_legend(show=TRUE, label="Age group: ") %>% 
    sg_axis_x(1, "year", "%Y")
    
  stream
  
  
# Life expectancy

  life_exp <- life_exp %>% 
  mutate(text = paste("Country: ", Country, "\nPopulation (M): ", Population,
                      "\nLife Expectancy: ", Life.expectancy,
                      "\nGdp: ", GDP, "\nBMI: ", BMI, sep="")) 
  
  life_exp_selected <- life_exp %>%  # Customize year
    filter(Year == "2015") 

  
  life_exp_plot <- ggplot(life_exp_selected,
         aes(x=Adult.Mortality, y=Life.expectancy, size = Population, color = BMI, 
             text=text)) +
    geom_point(alpha=0.7) +
    scale_size(range = c(1.4, 19), name="Population (M)") +
    theme(legend.position="none",panel.grid = element_blank())+
    labs(y = "Life expectancy (age)", x = "Adult mortality (per 1k population)")
    

  ggplotly(life_exp_plot, tooltip = "text")
  
  
# Save data 
  save(body_max, bodyfat, bodyfat_data, cancer, life_exp, stroke, suicide, 
       world_map_data, my_pal, file = "data.RData")
 