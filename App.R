# Load necessary libraries
# List of required packages
packages <- c("shiny", "leaflet", "geosphere", "dplyr")

# Install packages if they are not already installed
install_if_missing <- function(package) {
  if (!require(package, character.only = TRUE)) {
    install.packages(package, dependencies = TRUE)
    library(package, character.only = TRUE)
  }
}
install.packages("openxlsx")
install.packages("forecast")
install.packages("ggplot2")
install.packages("shinythemes")
# Install all required packages
lapply(packages, install_if_missing)

library(openxlsx)
# Load the dataset
#realestate_data <- read.xlsx("/Users/stephenvu9686/Documents/R project/thugiang/realestate_data.xlsx")  # Adjust the file path

# Load necessary libraries
library(shinythemes)
library(shiny)
library(leaflet)
library(geosphere)
library(dplyr)
library(forecast)
library(ggplot2)

# Load the dataset
current_data <- read.xlsx("/Users/stephenvu9686/Documents/R project/thugiang/realestate_data.xlsx") # Adjust the file path
historical_data <- read.xlsx("/Users/stephenvu9686/Documents/R project/thugiang/Rent_Prices.xlsx")  # Adjust the file path
historical_data <- historical_data %>%
  mutate(`Rent Price` = as.numeric(gsub("[^0-9]", "", `Rent.Price`)))

# Check for NAs in Rent Price after conversion
summary(historical_data$`Rent Price`)
historical_data <- historical_data %>%
  mutate(Date = as.Date(paste("01", Date), format = "%d %B %Y"))
print(historical_data)
# Define UI for the application
ui <- fluidPage(theme = shinytheme("superhero"),
                titlePanel("Rental Property Finder and Forecast"),
                sidebarLayout(
                  sidebarPanel(
                    textInput("postcode", "Enter Desired Postcode:", value = "4000"),
                    numericInput("income", "Enter Weekly Income:", value = 1000),
                    numericInput("year", "Select Year (e.g., 2023):", value = 2023),
                    sliderInput("slider", "Forecast confidence:", 1, 100, 30),
                    actionButton("search", "Find Rentals"),
                    actionButton("forecast", "Forecast Rent Prices")
                  ),
                  mainPanel(
                    leafletOutput("map"),
                    tableOutput("propertyTable"),
                    plotOutput("forecastPlot"),
                    textOutput("forecastValue"),style="color:red"  # Correctly add the text output here
                  )
                )
)

# Define server logic for the application
server <- function(input, output) {
  observeEvent(input$search, {
    # Function to calculate Haversine distance
    haversine_distance <- function(lat1, lon1, lat2, lon2) {
      dist <- distHaversine(c(lon1, lat1), c(lon2, lat2))
      return(dist / 1000)  # Convert to kilometers
    }
    
    # Find properties based on postcode
    filtered_data <- current_data %>%
      filter(postcode == as.numeric(input$postcode))
    
    # If the postcode exists in the dataset
    if (nrow(filtered_data) > 0) {
      lat_user <- filtered_data$latitude[1]
      lon_user <- filtered_data$longitude[1]
      
      # Calculate distance for each property and filter based on affordability
      suggestions <- current_data %>%
        mutate(distance = mapply(haversine_distance, latitude, longitude, lat_user, lon_user)) %>%
        filter(rentpw <= input$income * 0.3) %>%  # Assuming 30% of income for rent
        arrange(distance)
      
      # Output the table
      output$propertyTable <- renderTable({
        suggestions %>%
          select(streetAddress, locality, rentpw, distance) %>%
          head(10)  # Show the top 10 results
      })
      
      # Output the map
      output$map <- renderLeaflet({
        leaflet() %>%
          addTiles() %>%
          addMarkers(lng = suggestions$longitude, lat = suggestions$latitude,
                     popup = paste("Rent: $", suggestions$rentpw, "<br>",
                                   "Address: ", suggestions$streetAddress))
      })
    } else {
      output$propertyTable <- renderTable({
        data.frame(Message = "No properties found for the entered postcode.")
      })
    }
  })
  
  observeEvent(input$forecast, {
    # Filter historical data based on the selected year
    historical_filtered <- historical_data %>%
      filter(format(as.Date(Date, "%B %Y"), "%Y") == as.character(input$year))

    # Check if there is any data for the selected year
    if (nrow(historical_filtered) > 0) {
      # Convert the historical data into a time series object
      rent_ts <- ts(historical_filtered$`Rent Price`, start = c(input$year, 1), frequency = 12)

      # Apply a simple forecasting method (e.g., ARIMA)
      rent_forecast <- auto.arima(rent_ts)
      forecasted <- forecast(rent_forecast, h = 24)  # Forecasting next 5 years (60 months)
      forecast_year <- as.numeric(input$year)+1
      forecast_value <- forecasted$mean[which(time(forecasted$mean) == forecast_year)]
      print(forecast_value)
      output$forecastValue <- renderText({
        paste("Predicted rent price in", forecast_year, "is", forecast_value)
      })
      # Plot the forecast
      output$forecastPlot <- renderPlot({
        autoplot(forecasted) +
          labs(title = "Rent Price Forecast for the Next 5 Years",
               x = "Time", y = "Rent Price") +
          theme_minimal()
      })

    } else {
      print("No historical data available for the selected year")  # Debugging line
      print(historical_data)
      
      # Plot time series by different locations
      output$forecastPlot <- renderPlot({
        ggplot(historical_data, aes(x = 'Date', y = 'Rent Price', color = 'Place')) +
          geom_line() +
          labs(title = "Time Series of Rent Prices by Location",
               x = "Date", y = "Rent Price") +
          theme_minimal()
      })
    }
  })
}

# Run the application
shinyApp(ui = ui, server = server)