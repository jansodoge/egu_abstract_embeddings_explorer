# EGU24 Abstract Explorer

Welcome to the EGU24 Abstract Explorer! This repository contains the code for an R-Shiny based web application designed to simplify the process of discovering relevant presentations and posters at the European Geosciences Union General Assembly 2024 (#EGU24). With over 15,000 abstracts to sift through, this app utilizes novel natural language processing techniques, specifically sentence embeddings, to help users find abstracts with similar content.

### Features

- **Semantic Search**: Users can select a specific abstract, and the app provides a curated list of the most similar abstracts also present at EGU24.
  
- **Interactive Visualization**: The app offers an intuitive interface for exploring abstracts through interactive visualizations.

- **Self-hosting**: All of the code is open-source, allowing users to download, modify, and host the app for their organization. The underlying data can be provided upon request.

### Getting Started

To explore the app, visit the hosted version [here](https://jsodoge.shinyapps.io/EGU-Embeddings-Explorer/).

To self-host the app:

1. Clone this repository to your local machine.
2. Modify the code as needed to tailor it to your specific requirements or adapt it for another conference.
3. Use the provided R code for direct deployment to platforms like shinyapps.io without additional steps.
4. Refer to the documentation in the `app.R` file for further guidance.

Additionally, the `scraper.R` file provides a web-scraper to collect abstracts and additional information from the EGU website, facilitating data collection for future versions of the app.

### Contribution

This project embraces an open-source approach, encouraging contributions from the community. If you have ideas for improvement or new features, feel free to implement them and contribute directly to the repository. Together, we can enhance the app and make it even more valuable for users worldwide.

