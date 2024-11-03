library(elmer)
library(shiny)
library(shinychat)
library(bslib)
library(here)
library(dotenv)
library(purrr)
library(dplyr)
library(stringr)
library(glue)

# UI Components ----------------------------------------

create_orbit_form <- function() {
  sidebar(
    selectInput(
      inputId = "prompt_type",
      label = "Prompt Type",
      choices = c("Text Only", "With Image", "With Math", "Cloze Deletion")
    ),
    conditionalPanel(
      condition = "input.prompt_type != 'Cloze Deletion'",
      textAreaInput(
        inputId = "question",
        label = "Question",
        rows = 2
      ),
      textAreaInput(
        inputId = "answer",
        label = "Answer",
        rows = 2
      )
    ),
    conditionalPanel(
      condition = "input.prompt_type == 'Cloze Deletion'",
      textAreaInput(
        inputId = "cloze_text",
        label = "Cloze Text",
        rows = 3,
        placeholder = "Enter text with {word} to create a blank"
      ),
      helpText("Use curly braces to mark words for deletion, e.g., 'The {capital} of France is {Paris}.'")
    ),
    conditionalPanel(
      condition = "input.prompt_type == 'With Image'",
      textInput(
        inputId = "image_url",
        label = "Image URL"
      )
    ),
    actionButton(
      inputId = "add_prompt",
      label = "Add Prompt",
      class = "btn-primary"
    ),
    hr(),
    actionButton(
      inputId = "generate_all",
      label = "Generate Combined HTML",
      class = "btn-success"
    )
  )
}

create_prompts_list <- function() {
  card(
    card_header("Added Prompts"),
    div(
      style = "max-height: 400px; overflow-y: auto;",
      uiOutput("prompts_list")
    ),
    card_header("Generated HTML Code"),
    verbatimTextOutput("html_code")
  )
}

create_orbit_preview <- function() {
  div(
    class = "p-3",
    div(
      style = "background: white; padding: 20px; border-radius: 8px; box-shadow: 0 2px 4px rgba(0,0,0,0.1);",
      htmlOutput("preview")
    )
  )
}

# HTML Generation -------------------------------------

create_prompt_html <- function(prompt) {
  if (prompt$type == "Cloze Deletion") {
    return(create_cloze_prompt_html(prompt))
  }

  base_template <- '      <orbit-prompt
        question="{question}"
        {attachments}
        answer="{answer}"
      ></orbit-prompt>'

  attachments <- if (prompt$type == "With Image") {
    glue('question-attachments="{prompt$image_url}"')
  } else {
    ""
  }

  glue(
    base_template,
    question = prompt$question,
    attachments = attachments,
    answer = prompt$answer
  )
}

create_cloze_prompt_html <- function(prompt) {
  glue('      <orbit-prompt cloze="{prompt$cloze_text}"></orbit-prompt>')
}

generate_orbit_html <- function(prompts) {
  if (length(prompts) == 0) {
    return("")
  }

  html_template <- '
<html>
  <head>
    <script type="module" src="https://js.withorbit.com/orbit-web-component.js"></script>
    <meta property="orbit:color" content="purple">
  </head>
  <body>
    <orbit-reviewarea>
{prompts}
    </orbit-reviewarea>
  </body>
</html>'

  prompts_html <- prompts |>
    map(create_prompt_html) |>
    paste(collapse = "\n")

  glue(html_template, prompts = prompts_html)
}

# Validation ----------------------------------------

validate_cloze_text <- function(text) {
  # Check for matching braces
  opening_braces <- str_count(text, "\\{")
  closing_braces <- str_count(text, "\\}")

  if (opening_braces != closing_braces) {
    return("Mismatched braces. Please ensure all { have matching }")
  }

  # Check for non-empty deletions
  if (str_detect(text, "\\{\\}")) {
    return("Empty deletions are not allowed. Add text between { }")
  }

  # Check for nested braces
  if (str_detect(text, "\\{[^}]*\\{")) {
    return("Nested deletions are not allowed")
  }

  return(NULL)
}

validate_prompt <- function(input) {
  if (input$prompt_type == "Cloze Deletion") {
    return(validate_cloze_text(input$cloze_text))
  }

  if (input$prompt_type != "Cloze Deletion" &&
    (str_length(input$question) == 0 || str_length(input$answer) == 0)) {
    return("Question and answer are required")
  }

  if (input$prompt_type == "With Image" && str_length(input$image_url) == 0) {
    return("Image URL is required for image prompts")
  }

  return(NULL)
}

# Helpers ------------------------------------------

create_prompt <- function(input) {
  if (input$prompt_type == "Cloze Deletion") {
    list(
      type = "Cloze Deletion",
      cloze_text = input$cloze_text
    )
  } else {
    list(
      type = input$prompt_type,
      question = input$question,
      answer = input$answer,
      image_url = if (input$prompt_type == "With Image") input$image_url else NULL
    )
  }
}

reset_inputs <- function(session, prompt_type) {
  if (prompt_type == "Cloze Deletion") {
    updateTextAreaInput(session, "cloze_text", value = "")
  } else {
    updateTextAreaInput(session, "question", value = "")
    updateTextAreaInput(session, "answer", value = "")
    if (prompt_type == "With Image") {
      updateTextInput(session, "image_url", value = "")
    }
  }
}

create_prompt_card <- function(prompt, index) {
  card(
    tags$div(
      style = "display: flex; justify-content: space-between; align-items: start;",
      tags$div(
        h5(paste("Prompt", index)),
        if (prompt$type == "Cloze Deletion") {
          list(
            p(strong("Type: "), "Cloze Deletion"),
            p(strong("Text: "), HTML(str_replace_all(
              prompt$cloze_text,
              "\\{([^}]+)\\}",
              "<mark>\\1</mark>"
            )))
          )
        } else {
          list(
            p(strong("Type: "), prompt$type),
            p(strong("Question: "), prompt$question),
            p(strong("Answer: "), prompt$answer),
            if (!is.null(prompt$image_url)) p(strong("Image URL: "), prompt$image_url)
          )
        }
      ),
      actionButton(
        inputId = paste0("remove_", index),
        label = "Remove",
        class = "btn-danger btn-sm"
      )
    )
  )
}

# Main App -----------------------------------------

ui <- page_navbar(
  title = "Orbit Prompt Builder",
  theme = bs_theme(preset = "minty", version = "5"),
  nav_panel(
    title = "Chat",
    chat_ui("chat"),
  ),
  nav_panel(
    title = "Edit",
    layout_sidebar(
      sidebar = create_orbit_form(),
      create_prompts_list()
    )
  ),
  nav_panel(
    title = "Preview",
    create_orbit_preview()
  ),
  fillable = TRUE
)

server <- function(input, output, session) {
  # Initialize reactive values
  prompts <- reactiveVal(list())

  # Chat functionality
  chat <- elmer::chat_claude(
    model = "claude-3-5-sonnet-20241022",
    system_prompt = paste(readLines(here("system_prompt.md")), collapse = "\n")
  )

  chat_append("chat", paste(readLines(here("greeting.md")), collapse = "\n"))

  observeEvent(input$chat_user_input, {
    response <- chat$stream_async(input$chat_user_input)
    chat_append("chat", response)
  })

  # Orbit prompt management
  observeEvent(input$add_prompt, {
    validation_result <- validate_prompt(input)
    if (!is.null(validation_result)) {
      showNotification(validation_result, type = "error")
      return()
    }

    new_prompt <- create_prompt(input)
    prompts(c(prompts(), list(new_prompt)))
    reset_inputs(session, input$prompt_type)
  })

  output$prompts_list <- renderUI({
    if (length(prompts()) == 0) {
      return(p("No prompts added yet"))
    }

    prompts() |>
      imap(create_prompt_card) |>
      tags$div()
  })

  # Remove prompt handlers
  observe({
    prompts() |>
      seq_along() |>
      walk(~ {
        observeEvent(input[[paste0("remove_", .x)]], {
          prompts() %>%
            {
              .[-(.x)]
            } |>
            prompts()
        })
      })
  })

  # HTML generation and preview
  orbit_html <- reactive({
    prompts() %>%
      generate_orbit_html()
  })

  output$preview <- renderUI({
    HTML(orbit_html())
  })

  output$html_code <- renderText({
    orbit_html()
  })
}

shinyApp(ui, server)
