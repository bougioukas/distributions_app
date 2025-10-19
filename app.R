library(shiny)
library(ggplot2)

# Define UI for the application
ui <- fluidPage(
  # Enable MathJax for rendering LaTeX formulas
  withMathJax(),
  
  # Set up basic styling
  tags$head(
    tags$style(HTML("
            .irs-bar, .irs-bar-sat {
                background: #3B82F6; /* Blue-500 */
                border-top: 1px solid #2563EB;
                border-bottom: 1px solid #2563EB;
            }
            .irs-slider {
                border: 2px solid #2563EB;
            }
            .well {
                background-color = #F8FAFC; /* Light gray background for sidebar */
                border: none;
                border-radius: 0.5rem;
                box-shadow: 0 4px 6px -1px rgba(0, 0, 0, 0.1);
            }
            h2, h3 {
                color: #1E40AF; /* Dark Blue title */
                font-weight: 700;
            }
        "))
  ),
  
  # Application title
  headerPanel("Interactive Probability Distribution Visualizer"),
  
  sidebarLayout(
    # Sidebar for controls
    sidebarPanel(
      selectInput("distribution", "Select Distribution:",
                  choices = c("Normal", "Chi-Square", "t-Distribution", "F-Distribution"),
                  selected = "Normal"),
      
      # Dynamic UI output for distribution-specific parameters
      uiOutput("param_inputs"),
      
      # Legend for the new baseline
      uiOutput("legend_output")
    ),
    
    # Main panel for displaying the plot
    mainPanel(
      h3(textOutput("dist_title")),
      # New output for the PDF formula
      uiOutput("dist_formula"),
      plotOutput("dist_plot"),
      # New output for mean, variance, and key info (below plot)
      uiOutput("dist_properties")
    )
  )
)

# Define server logic
server <- function(input, output) {
  
  # 1. Dynamic Parameter Inputs (based on selected distribution)
  output$param_inputs <- renderUI({
    switch(input$distribution,
           "Normal" = list(
             p("The Normal distribution is defined by its mean (\u03bc) and standard deviation (\u03c3). Baseline is Standard Normal (\u03bc=0, \u03c3=1)."),
             sliderInput("norm_mean", "Mean (\u03bc):", min = -5, max = 5, value = 0, step = 0.5),
             sliderInput("norm_sd", "Standard Deviation (\u03c3):", min = 0.1, max = 5, value = 1, step = 0.1)
           ),
           "Chi-Square" = list(
             # Changed label from (k) to (\u03bd)
             p("The Chi-Square distribution is defined by its degrees of freedom (\u03bd). Baseline \u03bd = 5."),
             sliderInput("chisq_df", "Degrees of Freedom (\u03bd):", min = 1, max = 30, value = 5, step = 1)
           ),
           "t-Distribution" = list(
             p("The t-Distribution is defined by its degrees of freedom (\u03bd). As \u03bd increases, it approaches the Normal distribution. Baseline \u03bd = 10."),
             sliderInput("t_df", "Degrees of Freedom (\u03bd):", min = 1, max = 30, value = 10, step = 1),
             checkboxInput("t_show_normal", "Show Standard Normal for Comparison", value = TRUE)
           ),
           "F-Distribution" = list(
             p("The F-Distribution is defined by two degrees of freedom parameters. Baseline \u03bd\u2081=5, \u03bd\u2082=10."),
             sliderInput("f_df1", "Numerator df (\u03bd\u2081):", min = 1, max = 60, value = 5, step = 1),
             sliderInput("f_df2", "Denominator df (\u03bd\u2082):", min = 1, max = 60, value = 10, step = 1)
           )
    )
  })
  
  # Dynamic Legend Display (based on selected distribution)
  output$legend_output <- renderUI({
    if (input$distribution == "t-Distribution") {
      tags$div(
        style = "margin-top: 20px; padding: 10px; border-top: 1px solid #ddd;",
        tags$p(tags$strong("Plot Legend:")),
        tags$p(style="color: #1D4ED8;", tags$span("\u25AC\u25AC\u25AC"), " Current Parameters (Interactive)"),
        tags$p(style="color: #6B7280;", tags$span("\u00B7\u00B7\u00B7"), " Baseline Reference (Fixed)"),
        tags$p(style="color: #10B981;", tags$span("\u2014\u2014\u2014"), " Standard Normal")
      )
    } else {
      tags$div(
        style = "margin-top: 20px; padding: 10px; border-top: 1px solid #ddd;",
        tags$p(tags$strong("Plot Legend:")),
        tags$p(style="color: #1D4ED8;", tags$span("\u25AC\u25AC\u25AC"), " Current Parameters (Interactive)"),
        tags$p(style="color: #6B7280;", tags$span("\u00B7\u00B7\u00B7"), " Baseline Reference (Fixed)")
      )
    }
  })
  
  # 2. Dynamic Title for the Plot
  output$dist_title <- renderText({
    paste0("Probability Density Function of the ", input$distribution, " Distribution")
  })
  
  # 3. Dynamic Formula Display (using MathJax/LaTeX)
  output$dist_formula <- renderUI({
    formula_latex <- switch(input$distribution,
                            "Normal" = 
                              "$$f(x | \\mu, \\sigma) = \\frac{1}{\\sigma\\sqrt{2\\pi}} e^{-\\frac{1}{2} (\\frac{x-\\mu}{\\sigma})^2}$$",
                            
                            # Changed k to \nu in formula
                            "Chi-Square" = 
                              "$$f(x | \\nu) = \\frac{1}{2^{\\nu/2}\\Gamma(\\nu/2)} x^{\\nu/2 - 1} e^{-x/2} \\quad \\text{for } x > 0$$",
                            
                            "t-Distribution" = 
                              "$$f(t | \\nu) = \\frac{\\Gamma((\\nu+1)/2)}{\\sqrt{\\nu\\pi}\\Gamma(\\nu/2)} \\left(1 + \\frac{t^2}{\\nu}\\right)^{-(\\nu+1)/2}$$",
                            
                            "F-Distribution" = 
                              "$$f(x | \\nu_1, \\nu_2) = \\frac{\\left(\\frac{\\nu_1}{\\nu_2}\\right)^{\\nu_1/2} x^{\\nu_1/2 - 1}}{B(\\frac{\\nu_1}{2}, \\frac{\\nu_2}{2}) \\left(1 + \\frac{\\nu_1}{\\nu_2}x\\right)^{(\\nu_1+\\nu_2)/2}} \\quad \\text{for } x > 0$$"
    )
    
    # Display the formula using tags
    tagList(
      tags$h4(style="color: #1E40AF; margin-top: 15px; margin-bottom: 0px;", "Probability Density Function (PDF)"),
      withMathJax(formula_latex)
    )
  })
  
  # 4. Density Plot Generation
  output$dist_plot <- renderPlot({
    dist_name <- input$distribution
    
    # Generate data points for both interactive and baseline curves
    data_results <- switch(dist_name,
                           "Normal" = {
                             # Determine range based on interactive inputs
                             x_min <- input$norm_mean - 4 * input$norm_sd
                             x_max <- input$norm_mean + 4 * input$norm_sd
                             x_vals <- seq(x_min, x_max, length.out = 500)
                             
                             # Interactive Data
                             plot_data <- data.frame(
                               x = x_vals,
                               y = dnorm(x_vals, mean = input$norm_mean, sd = input$norm_sd)
                             )
                             
                             # Baseline Data (Standard Normal: mu=0, sd=1)
                             baseline_data <- data.frame(
                               x = x_vals,
                               y = dnorm(x_vals, mean = 0, sd = 1)
                             )
                             # Parameter Text for Normal
                             param_text <- paste0("Current Parameters:\n\u03bc = ", input$norm_mean, "\n\u03c3 = ", input$norm_sd)
                             list(plot_data = plot_data, baseline_data = baseline_data, param_text = param_text)
                           },
                           "Chi-Square" = {
                             # Determine range based on interactive inputs
                             x_max <- qchisq(0.999, df = input$chisq_df)
                             x_vals <- seq(0, x_max, length.out = 500)
                             
                             # Interactive Data
                             plot_data <- data.frame(
                               x = x_vals,
                               y = dchisq(x_vals, df = input$chisq_df)
                             )
                             
                             # Baseline Data (df=5)
                             baseline_data <- data.frame(
                               x = x_vals,
                               y = dchisq(x_vals, df = 5)
                             )
                             # Parameter Text for Chi-Square (updated to nu)
                             param_text <- paste0("Current Parameters:\n\u03bd = ", input$chisq_df)
                             list(plot_data = plot_data, baseline_data = baseline_data, param_text = param_text)
                           },
                           "t-Distribution" = {
                             # Determine range based on interactive inputs
                             x_max <- qt(0.9995, df = input$t_df)
                             x_min <- -x_max
                             x_vals <- seq(x_min, x_max, length.out = 500)
                             
                             # Interactive Data
                             plot_data <- data.frame(
                               x = x_vals,
                               y = dt(x_vals, df = input$t_df)
                             )
                             
                             # Baseline Data (df=10)
                             baseline_data <- data.frame(
                               x = x_vals,
                               y = dt(x_vals, df = 10)
                             )
                             
                             # Normal comparison data (Standard Normal N(0,1))
                             normal_data <- data.frame(
                               x = x_vals,
                               y = dnorm(x_vals, mean = 0, sd = 1)
                             )
                             
                             # Parameter Text for t-Distribution (already using nu)
                             param_text <- paste0("Current Parameters:\n\u03bd = ", input$t_df)
                             list(plot_data = plot_data, baseline_data = baseline_data, normal_data = normal_data, param_text = param_text)
                           },
                           "F-Distribution" = {
                             # Determine range based on interactive inputs
                             x_max <- qf(0.999, df1 = input$f_df1, df2 = input$f_df2)
                             x_vals <- seq(0, x_max, length.out = 500)
                             
                             # Interactive Data
                             plot_data <- data.frame(
                               x = x_vals,
                               y = df(x_vals, df1 = input$f_df1, df2 = input$f_df2)
                             )
                             
                             # Baseline Data (df1=5, df2=10)
                             baseline_data <- data.frame(
                               x = x_vals,
                               y = df(x_vals, df1 = 5, df2 = 10)
                             )
                             # Parameter Text for F-Distribution (already using nu)
                             param_text <- paste0("Current Parameters:\n\u03bd\u2081 = ", input$f_df1, "\n\u03bd\u2082 = ", input$f_df2)
                             list(plot_data = plot_data, baseline_data = baseline_data, param_text = param_text)
                           }
    )
    
    plot_data <- data_results$plot_data
    baseline_data <- data_results$baseline_data
    normal_data <- data_results$normal_data # May be NULL for non-t distributions
    param_text <- data_results$param_text # Extract the text
    
    # Create the ggplot plot
    p <- ggplot(plot_data, aes(x = x, y = y)) +
      
      # 1. Add the baseline reference line (light gray, dotted)
      geom_line(data = baseline_data, aes(x = x, y = y),
                color = "#6B7280", linetype = "dotted", linewidth = 1.2) +
      
      # 2. Add the interactive plot line (dark blue, solid)
      geom_line(color = "#1D4ED8", linewidth = 1.8) +
      
      labs(
        x = "Value (X)",
        y = "Probability Density f(X)"
      ) +
      # Use a clean, educational theme
      theme_minimal(base_size = 14) +
      theme(
        plot.title = element_text(hjust = 0.5, face = "bold", size = 18),
        axis.title = element_text(face = "italic"),
        # Subtle background
        panel.background = element_rect(fill = "#ECF2F9", color = NA),
        plot.background = element_rect(fill = "white", color = NA)
      )
    
    # Add Normal overlay for t-Distribution if checkbox is selected
    if (dist_name == "t-Distribution" && !is.null(normal_data) && input$t_show_normal) {
      p <- p + geom_line(data = normal_data, aes(x = x, y = y),
                         color = "#10B981", linewidth = 1.5, alpha = 0.8) # Green line for Normal
    }
    
    # Add mean/median/mode reference lines for context
    if (dist_name == "Normal") {
      p <- p + geom_vline(xintercept = input$norm_mean, linetype = "dashed", color = "#EF4444", linewidth = 1) # Red line at Mean
    } else if (dist_name == "t-Distribution") {
      p <- p + geom_vline(xintercept = 0, linetype = "dashed", color = "#EF4444", linewidth = 1) # Red line at Mean/Median
    } else if (dist_name == "Chi-Square" && input$chisq_df > 2) {
      # Mode of Chi-square for df > 2 is df - 2
      p <- p + geom_vline(xintercept = input$chisq_df - 2, linetype = "dashed", color = "#EF4444", linewidth = 1) # Red line at Mode
    }
    
    # 4. Add Parameter Annotation (Top Right Corner)
    p <- p + annotate("text", 
                      x = Inf, y = Inf, 
                      label = param_text, 
                      hjust = 1.05, vjust = 1.2, 
                      size = 5, fontface = "bold", 
                      color = "#1E40AF") # Dark blue text
    
    print(p)
  })
  
  # 5. Dynamic Properties Display (Mean, Variance, Key Info) - BELOW PLOT
  output$dist_properties <- renderUI({
    properties_data <- switch(input$distribution,
                              "Normal" = list(
                                mean_formula = "\\(\\mu\\)",
                                variance_formula = "\\(\\sigma^2\\)",
                                info = "Symmetric, bell-shaped curve. Central Limit Theorem: sample means approximate normal distribution. Used extensively in hypothesis testing and confidence intervals."
                              ),
                              "Chi-Square" = list(
                                mean_formula = "\\(\\nu\\)",
                                variance_formula = "\\(2\\nu\\)",
                                info = "Right-skewed distribution, used in goodness-of-fit tests and variance estimation. Represents the sum of squared standard normal random variables. Common in categorical data analysis."
                              ),
                              "t-Distribution" = list(
                                mean_formula = "\\(0\\) (for \\(\\nu > 1\\))",
                                variance_formula = "\\(\\frac{\\nu}{\\nu - 2}\\) (for \\(\\nu > 2\\))",
                                info = "Symmetric with heavier tails than normal. Used for small sample inference when population variance is unknown. Converges to N(0,1) as ν → ∞. Critical for t-tests."
                              ),
                              "F-Distribution" = list(
                                mean_formula = "\\(\\frac{\\nu_2}{\\nu_2 - 2}\\) (for \\(\\nu_2 > 2\\))",
                                variance_formula = "\\(\\frac{2\\nu_2^2(\\nu_1 + \\nu_2 - 2)}{\\nu_1(\\nu_2 - 2)^2(\\nu_2 - 4)}\\) (for \\(\\nu_2 > 4\\))",
                                info = "Right-skewed distribution used in ANOVA and regression analysis. Represents the ratio of two independent chi-square variables divided by their degrees of freedom. Essential for comparing variances."
                              )
    )
    
    withMathJax(
      tags$div(
        style = "background-color: #F0F9FF; padding: 20px; border-radius: 8px; margin-top: 20px; border-left: 4px solid #3B82F6;",
        tags$h4(style = "color: #1E40AF; margin-top: 0px; margin-bottom: 15px;", "Distribution Properties"),
        tags$div(
          style = "display: grid; grid-template-columns: 1fr 1fr; gap: 20px; margin-bottom: 15px;",
          tags$div(
            tags$strong(style = "color: #1E40AF;", "Mean (μ): "),
            HTML(properties_data$mean_formula)
          ),
          tags$div(
            tags$strong(style = "color: #1E40AF;", "Variance (σ²): "),
            HTML(properties_data$variance_formula)
          )
        ),
        tags$div(
          style = "border-top: 1px solid #BFDBFE; padding-top: 15px;",
          tags$strong(style = "color: #1E40AF;", "Key Properties: "),
          tags$span(style = "color: #374151;", properties_data$info)
        )
      )
    )
  })
}

shinyApp(ui = ui, server = server)