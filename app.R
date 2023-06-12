library(markdown)
library(shiny)
library(ggplot2)
library(bslib)
library(plotly)
library(ggthemr)
library(shinythemes)
library(fmsb)
library(maps)
library(streamgraph)
library(dplyr)
library(ggthemes)

# Load data
load("data.Rdata")

# Load theme
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

# Define UI for application that draws a histogram
# Define UI for random distribution app ----
ui <- fluidPage(
  
  # Set theme
  theme = bs_theme(version = 4, bootswatch = "minty"),
  
  # Rshiny
  navbarPage("Health Vitality App",
             tabPanel("How likely will you have stroke?",
                      sidebarLayout(
                        sidebarPanel(
                          selectInput("gender", "What is your gender ?",
                                      choices = unique(stroke$gender)),
                          selectInput("class", "Which age group do you belong to ?",
                                      choices = unique(stroke$class)),
                          selectInput("hyper", 
                                      "Do you have hypertension (0 for No, 1 for Yes)",
                                      choices = unique(stroke$hypertension)),
                          selectInput("marital", "Have you ever been married ?",
                                      choices = unique(stroke$ever_married)),
                          selectInput("glucose", "What is your glucose level ?",
                                      choices = unique(stroke$glucose_level)),
                          selectInput("smoke", "How often do you smoke ?",
                                      choices = unique(stroke$smoking_status))
                        ),
                        mainPanel(plotlyOutput("stroke_pie_plot")
                          
                        )
                      )
             ),
             tabPanel("What is your bodyfat percent ?",
                      sidebarLayout(
                        sidebarPanel(
                          sliderInput("age_type", "What is your age ?", value = 20,
                                      min = 0, max = 81),
                          selectInput("age", "Which age group do you belong to ?",
                                      choices = unique(bodyfat$age_group)),
                          sliderInput("weight_type", "What is your weight ?", value = 200,
                                      min = 0, max = 262.75),
                          selectInput("weight", "Which weight group do you belong to ?",
                                      choices = unique(bodyfat$weight_class)),
                          sliderInput("height_type", "What is your height ?", value = 40,
                                      min = 0, max = 77.75),
                          selectInput("height", "Which height group do you belong to ?",
                                      choices = unique(bodyfat$height_class)),
                          sliderInput("chest_type", "What is your chest circumference ?",
                                      value = 70, min = 0, max = 128.3),
                          selectInput("chest", 
                                      "What is your chest circumference range ?",
                                      choices = unique(bodyfat$chest_group)),
                          sliderInput("ab_type", "What is your abdomen circumference ?",
                                      value = 80, min = 0, max = 126.2),
                          selectInput("ab", 
                                      "What is your abdomen circumference range ?",
                                      choices = unique(bodyfat$abdomen_group)),
                          sliderInput("hip_type", "What is your hip circumference ?",
                                      value = 100, min = 0, max = 125.6),
                          selectInput("hip", 
                                      "What is your hip circumference range ?",
                                      choices = unique(bodyfat$hip_group)),
                          sliderInput("thigh_type", "What is your thigh circumference ?",
                                      value = 50, min = 0, max = 74.4),
                          selectInput("thigh", 
                                      "What is your thigh circumference range ?",
                                      choices = unique(bodyfat$thigh_group))

                        ),
                        mainPanel(plotlyOutput("bodyfat_radar_plot", height = "800px")
                          
                        )
                      )
             ),
             tabPanel("Cancer mortality liability around the world",
                      sidebarLayout(
                        sidebarPanel(
                          selectInput("year", "Select the year you want to display"
                                      ,choices = unique(cancer$Period)),
                          selectInput("sex", "Select the gender group you want to display"
                                      ,choices = unique(cancer$Sex))
                        ),
                        mainPanel(plotlyOutput("cancer_map_plot")
                        )
                      )     
             ),
             tabPanel("Suicide rate around the world",
                      sidebarLayout(
                        sidebarPanel(
                          selectInput("country", "Select the country you want to display"
                                      ,choices = unique(suicide$country)),
                          sliderInput("years", "Select the year you want to display",
                                      value = c(min(suicide$year), max(suicide$year)),step=1,
                                      sep="",
                                      ,min = min(suicide$year), max = max(suicide$year)),
                          selectInput("fm", "Select the gender group you want to display"
                                      ,choices = unique(suicide$sex))
                        ),
                        mainPanel(streamgraphOutput("suicide_stream_graph"))
                      )     
             ),    
             tabPanel("Life expectancy around the world",
                      sidebarLayout(
                        sidebarPanel(
                          selectInput("yr", "Select the year you want to display"
                                      ,choices = unique(life_exp$Year)),
                          selectInput("nation", "Select the country you want to display"
                                      ,choices = c("All", unique(life_exp$Country)))
                        ),
                        mainPanel(plotlyOutput("life_exp_plot_output"))
                      )     
             )
  )
)


# Define server logic required to draw a histogram
server <- function(input, output , session) {
  
  # Plot stroke_pie
  stroke_pie <- reactive({
    # Choose the parent theme group (rshiny input)
    test_stroke<- stroke %>% 
      filter(gender == input$gender,
             class == input$class,
             hypertension == input$hyper,
             ever_married == input$marital,
             glucose_level ==  input$glucose,
             smoking_status == input$smoke) %>% 
      arrange(stroke) %>%
      count(stroke) %>% 
      mutate(label = "N/A")
    
    validate(
      need(is.na(test_stroke)==FALSE, message=paste(
        "Sorry, we cannot predict your stroke liability at this time"))
    )
    
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
    
    
  })
  
  # Output of theme_bar_polar
  output$stroke_pie_plot <- renderPlotly(stroke_pie())
  
  
  
  # Plot bodyfat_radar

  bodyfat_radar <- reactive({
    
    # Filtering information 
    test_bodyfat <- bodyfat %>% 
      filter(age_group == input$age,
             weight_class == input$weight,
             height_class == input$height,
             chest_group == input$chest,
             abdomen_group == input$ab,
             hip_group == input$hip,
             thigh_group == input$thigh
             )%>% 
      arrange(BodyFat) %>%
      mutate(Approximation = mean(BodyFat)) 
    
    body_max["You",1] <- test_bodyfat$Approximation[1]
    
    body_max["You",2:8] <- c(input$age_type, input$weight_type,
                          input$height_type, input$chest_type, input$ab_type,
                          input$hip_type, input$thigh_type)
    
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
    
  })
  
  # Output of bodyfat_radar
  output$bodyfat_radar_plot <- renderPlotly(bodyfat_radar())
  
  
  
  # Plot of cancer_map
  cancer_map <- reactive({
    
    cancer_select <- cancer %>% 
      filter(Period == input$year) %>% 
      right_join(world_map_data, by = c("Country"="region")) %>% 
      select(-subregion) %>% 
      filter(Sex == input$sex)
    
    map_plot <- ggplot() +
      geom_polygon(aes(x=long,y=lat,group=group, fill=Liability, subgroup = Country),
                   data=cancer_select)+ 
      theme_map()+
      coord_quickmap()+
      labs(title="Cancer mortality liability for each country (%)") +
      theme(plot.title = element_text(hjust=0.5),
            plot.subtitle = element_text(hjust=0.5),
            legend.position = "none") +
      scale_fill_gradient(name="Cancer liability (%)",low="dodgerblue",
                          high="salmon")
    
    ggplotly(map_plot, tooltip = c("fill","subgroup"))

  })
  
  # Output of cancer_map
  output$cancer_map_plot <- renderPlotly(cancer_map())
  
  
  # Plot of part_bar
  observe({
    suicide_selected <- suicide %>% 
      select(-country.year, -HDI.for.year) %>% 
      filter(country == input$country)
    updateSelectInput(session,"years",choices = unique(suicide_selected$year))
  })
  
  suicide_stream <- reactive({
    
    # Filter by country, year
    suicide_selected <- suicide %>% 
      select(-country.year, -HDI.for.year) %>% 
      filter(country == input$country &
             as.numeric(year)>input$years[1] & as.numeric(year)<input$years[2] &
             sex == input$fm) %>%
      group_by(year, age)
    
    sg <- streamgraph(suicide_selected,"age", "suicides.100k.pop", "year") %>%
      sg_legend(show=TRUE, label="Age group: ") %>% 
      sg_axis_x(1, "year", "%Y")
    
    
    sg
    

  })
  
  # Output of suicide_stream
  output$suicide_stream_graph <- renderStreamgraph(suicide_stream())
  
  
  
  # Plot of set_prod_plot
  life_exp_plot <- reactive({
    
    life_exp_selected <- life_exp %>%  # Customize year
      filter(Year == input$yr)
    
    if (input$nation != "All")
    {
      life_exp_selected <- life_exp_selected %>% 
        filter(Country == input$nation)
    }
    
    life_exp_plot <- ggplot(life_exp_selected,
                            aes(x=Adult.Mortality, y=Life.expectancy, 
                                size = Population, color = BMI, 
                                text=text)) +
      geom_point(alpha=0.7) +
      scale_size(range = c(1.4, 19), name="Population (M)") +
      theme(legend.position="none",panel.grid = element_blank())+
      labs(y = "Life expectancy (age)", x = "Adult mortality (per 1k population)")
    
    
    ggplotly(life_exp_plot, tooltip = "text")
    
  })
  
  # Output of set_prod_plot
  output$life_exp_plot_output <- renderPlotly(life_exp_plot())
  
}


# Run the application 
shinyApp(ui = ui, server = server)
