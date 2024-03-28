library(plotly)
library(shiny)
library(text2map)
library(gt)
library(shinythemes)
library(shinyWidgets)
library(tidyverse)
library(shinycssloaders)
library(readr)

make_hyperlink = function(myurl, mytext = myurl) {
  paste('<a href="', myurl, '" style="color: white;">', "EGU LINK", '</a>')
}

egu_2023_umaps <<- read_csv("egu_2024_umaps.csv")
egu_2023_abstracts <<- read_csv("egu_2024_abstracts.csv")
points_data <<- egu_2023_umaps %>% 
  dplyr::left_join(egu_2023_abstracts, by = c("id_abstract" = "id_doc"))


ui <- fluidPage(
  
  theme = shinytheme("superhero"),
  tags$head(tags$style(
    HTML(
      "
      mark {
        font-family: bold;
        color: red;
      }
      hr {border-top: 1px solid #000000;}
    "
    )
  )),
  
  # Application title
  #titlePanel("#EGU24 Find your abstract"),
  htmlOutput("intro_text"),
  
  # Search input and similar abstracts table
  fluidRow(
    column(width = 12,
           pickerInput(
             inputId = "abstract_id",
             selected = "State of a challenge – Third annual review",
             label = tags$span("Search by title:", style = "font-size: 20px;"),
             choices = points_data$title,
             options = list(
               `live-search` = TRUE
             ),
             multiple = FALSE,
             width = "100%"
           ),
           numericInput("n_entries", 
                        label = tags$span("Number of Entries to Show (max. 200):", 
                                          style = "font-size: 20px;"),
                        value = 20, 
                        min = 1, 
                        max = 200, 
                        width = "100%"),
           withSpinner(gt_output("similiar_abstracts"))
    )
  ),
  
  # Plot area
  fluidRow(
    column(width = 12,
           div(style = "border: 2px solid white; padding: 20px;",
           shiny::h2("A an interactive landscape of all abstracts", style = "text-align: center;"), 
           div(style = "text-align: center;",
               actionButton("showPlotBtn", "Click to load", style = "font-size: 20px;")),
           conditionalPanel(
             condition = "input.showPlotBtn > 0",
           
           
           
           htmlOutput("scatter_text"),
           plotlyOutput("scatterplot", height = "1000px")))
    )
  ),
  
  # Bottom text
  fluidRow(
    column(width = 12,
           htmlOutput("bottom_text")
    )
  )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
  
  
  
  egu_2023_embeddings <- read_csv("egu_2024_embeddings.csv") %>% 
    dplyr::select(!...1) %>% 
    as.matrix()
  rownames(egu_2023_embeddings) <- points_data$title
  
  
  output$scatterplot <- renderPlotly({
    p <- plot_ly(points_data, x = ~umap_x, y = ~umap_y, type = 'scatter', mode = 'markers', text = ~full_citation_,
                 marker = list(color = "#add8e6")) %>%
      layout(
        xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE), 
        yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE), 
        plot_bgcolor = 'rgba(0,0,0,0)', 
        paper_bgcolor = 'rgba(0,0,0,0)', 
        legend = list(orientation = "h", x = 0, y = 1.1), 
        margin = list(l = 20, r = 20, t = 20, b = 20), 
        shapes = list(
          list(
            type = 'rect',
            fillcolor = 'rgba(255,255,255,0)', 
            line = list(color = 'white', width = 2),
            x0 = min(points_data$umap_x) - 0.5, 
            y0 = min(points_data$umap_y) - 0.5, 
            x1 = max(points_data$umap_x) + 0.5, 
            y1 = max(points_data$umap_y) + 0.5 
          )
        )
      )
    
    selected_index <- which(points_data$title == input$abstract_id)
    
    p <- add_trace(p, data = points_data[selected_index, ], color = I("red"), marker = list(size = 12, color = "red"),
                   showlegend = FALSE, 
                   text = points_data[selected_index, "full_citation_"])
    
    p
    
  })
  
  output$similiar_abstracts <- render_gt({
    embeddings <- egu_2023_embeddings
    cos_sim <- text2vec::sim2(
      x = embeddings,
      y = embeddings[input$abstract_id, , drop = FALSE],
      method = "cosine"
    )
    tmp <- head(sort(cos_sim[, 1], decreasing = TRUE), input$n_entries) %>% 
      as.data.frame()
    
    colnames(tmp)[colnames(tmp) == '.'] <- 'similarity'
    
    tmp$abstract <- rownames(tmp)
    tmp %>% 
      dplyr::left_join(egu_2023_abstracts %>% dplyr::select(title, division, id_doc, full_citation_), by = c("abstract"= "title")) %>% 
      dplyr::mutate(egu_link = paste0("https://meetingorganizer.copernicus.org/EGU24/EGU24-",id_doc,".html")) %>% 
      dplyr::rename(title = abstract) %>% 
      dplyr::select(!id_doc) %>%
      dplyr::select(!title) %>% 
      mutate(full_citation_ = str_replace(full_citation_, "How to cite:", "")) %>% 
      mutate(full_citation_ = str_replace(full_citation_, "EGU General Assembly 2024", "\nEGU General Assembly 2024")) %>% 
      tibble::rowid_to_column(var = "similarity_rank") %>% 
      dplyr:::select(similarity_rank, similarity, full_citation_, division, egu_link) %>% 
      gt() %>% 
     
        fmt(
          columns = 'egu_link',
          fns = make_hyperlink) %>% 
      tab_header(
        title = "Most similiar conference contributions"
      ) %>% 
      cols_label(similarity = "Similarity score",
                  egu_link = "EGU URL",
                  similarity_rank = "Rank",
                  division = "Division",
                  full_citation_ = "Abstract") %>% 
      tab_options(
        table.background.color = "#2b3e50",
        column_labels.font.size = px(15),
        column_labels.font.weight = "bold",
        table.font.color = "white",
        table.font.size = px(15L)
      ) 
  })
  
  
  output$intro_text <- renderUI({
    
    HTML('
        <h1>Find your abstract at #EGU24</h1>
        
        
        <p style="border-width:3px; border-style:solid; border-color:white; padding: 1em; font-size: 1.7em;">
        
        Hello there, <br> <br>
Feeling FOMO ( fear of missing out) about missing out on relevant presentations at EGU? Do not worry, 
we have got your back. Our web app simplifies the hunt for conference contributions similar to yours or any other.<br> <br>

 Here is how it works:<br> <br>
           1.           Start by typing and selecting keywords included the title of your own abstract at EGU.<br> <br>
         2.           Instantly, you will get a table of abstracts closely matching your selection.<br> <br>
3.           You will see a similarity score in the left column of the table, with the most similar abstracts sorted at the top.<br> <br>
         We calculate similarity using advanced natural language processing techniques. 
        In simple terms, we create digital fingerprints of each abstract based on its content and compare them to your selected one. 
        The underlying technique is called text embeddings. If you are curious you
can read more about it <a href="http://jsodoge.eu/post/2020-12-01-r-rmarkdown/">here</a> and <a href="https://www.sbert.net/">here</a>. <br><br>
       
        
        
        
         </p>')
  })
  
  
  
  output$scatter_text <- renderUI({
    
    HTML('
        
        <p style="border-width:3px; border-style:solid; border-color:white; padding: 1em; font-size: 1.7em;">
        
        Bored of using the table to find abstract? Well, here is a cool two-dimensional representation of all the abstracts at EGU. This is basically a map of all abstracts where each dot represents an abstract.
        Dots closer to each other are more similiar. The abstract which you selected is displayed in red. You can use this map to navigate to your abstract and explore its neighborhood on the map. Maybe
        you find another relevant contribution which you did not spot in the table. This map will probably display some different results to the table as it holds a dimensionality reduction of 500 dimensions to 2 
        dimensions for this visualization. Insead, the similarity score in the table above uses all 500 dimensions. 
         </p>')
  })
  
  
  
  output$bottom_text <- renderUI({    
    HTML('
        
        <p style="border-width:3px; border-style:solid; border-color:white; padding: 1em; font-size: 1.7em;">
        
        
        This app is the result of thoughts from the research group on computational hydrological extremes at the UFZ and has been developed Jan Sodoge. 
        
        All abstracts used are available with Creative Commons Attribution 4.0 License. 
        
        
       The code for the app is open source and data available upon request (at this point). If you have questions about self-hosting (see github.com/jansodoge/egu_abstract_embeddings_explorer) this application or anything else please contact jan.sodoge[at]ufz.de
        
        
         </p>')
  })
  
  
}

# Run the application 
shinyApp(ui = ui, server = server)
