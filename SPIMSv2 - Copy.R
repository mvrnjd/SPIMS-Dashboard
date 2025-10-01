# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    https://shiny.posit.co/
#Should admin defalt table show first submission or most recent submission on top?
#Should the updated shortage pull from first preference or total number of submissions

library(shiny)
library(shinydashboard)
library(tidyverse)
library(bslib)
library(leaflet)
library(leaflet.extras)
library(bsicons)
library(htmltools)
library(DT)
library(thematic)
library(arrow)
library(forcats)
library(fontawesome)
library(shinyjs)
library(shinyauthr)
library(dplyr)
library(DBI)
library(shinyalert)
library(tinytex)


# Load datasets
# Need to add import lines for the images...depends on how it will be integrated into STRIDE Dashboard
school <- read.csv("School-Unique-v2.csv")
user_data <- read.csv("User-Test-Data.csv")

#Adding database path for survey
db_path <- "SPIMS_data.sqlite"

init_db <- function() {
  conn <- dbConnect(RSQLite::SQLite(), db_path)
  # Create the survey entries table
  
  dbExecute(conn, "DROP TABLE IF EXISTS school_counts")
  dbExecute(conn, "DROP TABLE IF EXISTS entries")
  
  dbExecute(conn, "
  CREATE TABLE IF NOT EXISTS entries (
    id INTEGER PRIMARY KEY AUTOINCREMENT,
    user_name TEXT,
    applicant_code TEXT,
    school1_id TEXT,
    school2_id TEXT,
    school3_id TEXT,
    school4_id TEXT,
    school5_id TEXT,
    user_prov TEXT,
    user_mun TEXT,
    user_level TEXT,        -- FIX: TEXT (not TEST)
    timestamp DATETIME DEFAULT CURRENT_TIMESTAMP
  )
")
  
  # Create the school_counts  to track number of SPIMS applicants to each school
  #If you want to alter this table, it is easiest to delete it and re-create it so it does not get crowded
  dbExecute(conn, "
  CREATE TABLE IF NOT EXISTS school_counts (
    school_id TEXT PRIMARY KEY,
    school    TEXT,              -- add OR remove all uses of it
    municipality TEXT,
    number_applicants INTEGER DEFAULT 0,
    first_pref_schools INTEGER DEFAULT 0
  )
")
  
  # Populate school_counts with schools from the Schools-Unique-v2
  for (i in 1:nrow(school)) {
    dbExecute(conn, "
  INSERT OR IGNORE INTO school_counts (school_id, school, municipality, number_applicants, first_pref_schools)
  VALUES (?, ?, ?, 0, 0)
", params = list(school$SchoolID[i], school$SchoolName[i], school$Municipality[i]))
    
  }
  
  dbDisconnect(conn)
}

# Call the function once during app startup (this next section, up until line 238 is the exact same as the STRIDE code but with another user added)
init_db()

user_base <- tibble::tibble(
  user = c("iamdeped", "depedadmin", "spimsuser", "sadmin"),
  password = c("deped123", "admin123", "spims123", "sadmin123"), # In a real app, use hashed passwords
  password_hash = sapply(c("deped123", "admin123", "spims123", "sadmin123"), sodium::password_store), # Hashed passwords
  permissions = c("admin", "standard", "spims", "spimsadmin"),
  name = c("User One", "User Two", "User Three", "User Four")
)

#Defining functions
textInputRO <- function(inputId, label, value = "") {
  tagList(
    tags$label(label, `for` = inputId),
    tags$input(id = inputId, type = "text", class = "form-control", value = value, readonly = NA)
  )
}

# Define UI for application that draws a histogram
ui <- fluidPage(
  
  theme = bs_theme(bootswatch = "litera",
                   version = 5,
                   base_font = font_google("Poppins"),
                   code_font = font_google("Poppins")),
  
  tags$head(
    tags$style(HTML("
    /* Add this to your tags$style(HTML(...)) block in ui.R */
.navbar {
    z-index: 9999 !important; /* Ensures the navbar is on top of everything */
}
/* Ensure the body can scroll if needed */
html, body {
    height: 100%;
    margin: 0;
    background-color: transparent;
    overflow-x: hidden; /* Allows vertical scroll but prevents horizontal if possible */
}
    
    /* 1. Reset Full page background and remove filter/shadow from body */
    html, body {
      height: 100%;
      margin: 0;
      /* Ensure the body is transparent to show the background div */
      background-color: transparent; 
       
    }
    
    /* 2. Dedicated Faded Background Element (NEW) */
    #faded_background {
      position: fixed;
      top: 0;
      left: 0;
      width: 100vw; 
      height: 100vh; 
      z-index: -1; /* Puts it behind everything */
      
      /* Set the background image */
      background-image: url('bg_pic.jpg'); /* ⚠️ REPLACE THIS URL */
      background-size: cover;
      background-position: center bottom;
      
      /* Apply Grayscale filter (black and white) */
      filter: grayscale(100%);
      
      /* Apply a subtle, faded dark blue color overlay (using a pseudo-element) */
    }
    
    /* Create the blue color overlay using a pseudo-element */
    #faded_background::after {
        content: '';
        position: absolute;
        top: 0;
        left: 0;
        width: 100%;
        height: 100%;
        background-color: rgba(0, 51, 153, 0.2); /* Semi-transparent blue */
        z-index: 0; /* Sits on top of the image in this div */
    }

      /* -- EXISTING LOGIN PANEL CSS REMAINS UNCHANGED -- */

      /* Scope the flex centering ONLY to login panel */
      #login_panel_wrapper {
        height: 100vh; /* full viewport height */
        display: flex;
        justify-content: center;
        align-items: center;
      }

      /* Login panel */
      #login_panel {
        /* Keep the login panel background opaque white so the text is readable */
        background: rgba(255, 255, 255, 0.95);
        padding: 30px 50px;
        border-radius: 20px;
        width: 420px;
        text-align: center;
        box-shadow: 0 8px 25px rgba(0, 0, 0, 0.25);
      }

      /* Logo */
      #login_logo {
        width: 80px;
        margin-bottom: 15px;
      }

      /* Title */
      #login_panel h4 {
        font-family: 'Poppins', sans-serif;
        font-weight: 700;
        color: #003399;
        margin-bottom: 20px;
      }

      /* Inputs */
      #login_panel input {
        border-radius: 10px;
        border: 1px solid #ccc;
        margin-bottom: 15px;
      }

      /* Button */
      #login_panel button {
        background: linear-gradient(90deg, #003399, #b22222);
        color: white;
        border-radius: 25px;
        padding: 10px 25px;
        font-weight: 600;
        transition: all 0.3s ease;
        border: none;
      }
      #login_panel button:hover {
        background: linear-gradient(90deg, #b22222, #003399);
        box-shadow: 0px 4px 12px rgba(0,0,0,0.3);
      }
    "))
  ),
  
  shinyjs::useShinyjs(),
  
  # NEW: The dedicated background div for the filtered image
  div(id = "faded_background"), 
  
  # Wrapper div for centering login ONLY
  div(
    id = "login_panel_wrapper",
    div(
      id = "login_panel",
      tags$img(src = "deped_logo.png", id = "login_logo"), # put logo in /www
      shinyauthr::loginUI(
        id = "login",
        title = "Please Log In",
        user_title = "Username",
        pass_title = "Password",
        login_title = "Log In",
        error_message = "Invalid username or password!"
      )
    )
  ),
  
  # NEW: Permanent container for the Logout button
  # It must be visible when the STRIDE content is visible.
  shinyjs::hidden(
    div(
      id = "logout_div",
      style = "position: absolute; top: 10px; right: 20px; z-index: 10000;", # Position it clearly
      shinyauthr::logoutUI(
        id = "logout",
        label = "Log Out",
        class = "btn btn-danger btn-sm"
      )
    )
  ),
  
  # Your main content outputs:
  shinyjs::hidden(div(id = "main_content", uiOutput("STRIDE1"))),
  shinyjs::hidden(div(id = "mgmt_content", uiOutput("STRIDE2"))),
  shinyjs::hidden(
    div(
      id = "spims_content", 
      uiOutput("STRIDE3"),
      style = "padding: 10px;" 
    )
  ),
  shinyjs::hidden(div(id = "spims_admin", uiOutput("STRIDE4")))
)




# Define server logic required to draw a histogram
server <- function(input, output, session) {
  
  # Call the shinyauthr::logoutServer module
  logout_init <- shinyauthr::logoutServer(
    id = "logout",
    active = reactive(credentials()$user_auth) # Logout button active only when logged in
  )
  
  # --- Authentication ---
  # Call the shinyauthr::loginServer module
  # credentials() will be a reactive returning a tibble with user_auth, info, and additional columns from user_base
  credentials <- shinyauthr::loginServer(
    id = "login",
    data = user_base,
    user_col = user,
    pwd_col = password_hash, # Use the hashed password column
    sodium_hashed = TRUE,    # Important: tell shinyauthr we are using sodium hashes
    log_out = reactive(logout_init()) # Link to the logout button
  )
  
  
  
  # --- Reactive Values & Observers ---
  # Observe the authentication status
  # --- Reactive Values & Observers ---
  # Observe the authentication status
  observe({
    auth_status <- credentials()$user_auth
    
    # Define all content IDs including the new logout container
    all_content_ids <- c("main_content", "mgmt_content", "spims_content", "spims_admin")
    all_content_ids_and_logout <- c(all_content_ids, "logout_div") # Include the new logout container
    
    # Hide all content and the logout button initially/on logout
    sapply(all_content_ids_and_logout, shinyjs::hide)
    
    if (auth_status) {
      shinyjs::hide("login_panel_wrapper")
      shinyjs::show("logout_div") # <--- NEW: Show the floating logout button
      
      # User is authenticated. Let's get their details.
      user_info <- credentials()$info 
      
      # Ensure user_info is available and has the username
      if (!is.null(user_info) && "user" %in% names(user_info)) {
        current_username <- user_info$user # Get the username
        
        # --- Conditional logic based on username ---
        if (current_username == "iamdeped") { 
          shinyjs::show("main_content")
          shinyjs::hide("mgmt_content")
          shinyjs::hide("spims_content")
          shinyjs::hide("spims_admin")
        } else if (current_username == "depedadmin") {
          shinyjs::show("mgmt_content")
          shinyjs::hide("main_content")
          shinyjs::hide("spims_content")
          shinyjs::hide("spims_admin")
        } else if (current_username == 'spimsuser') {
          shinyjs::show("spims_content") # This is what you want for 'spimsuser'
          shinyjs::hide("mgmt_content")
          shinyjs::hide("main_content")
          shinyjs::hide("spims_admin")
        } else if (current_username == 'sadmin') {
          shinyjs::hide("spims_content")
          shinyjs::hide("mgmt_content")
          shinyjs::hide("main_content")
          shinyjs::show("spims_admin")
        }
      }
    } else {
      # User is NOT authenticated (e.g., after logout or initially)
      shinyjs::show("login_panel_wrapper") # <--- ADD THIS LINE back for logout/initial state
      shinyjs::hide("main_content")
      shinyjs::hide("mgmt_content")
      shinyjs::hide("spims_content")
      shinyjs::hide("spims_admin")
      
      # Reset inputs on logout
      updateTextInput(session, "user_prov", value = "")
      updateTextInput(session, "user_mun", value = "")
      updateTextInput(session, "user_mun_2", value = "")
      updateTextInput(session, "user_mun_3", value = "")
      updateTextInput(session, "user_level", value = "")
      updateTextInput(session, "user_name", value = "")
      updateTextInput(session, "applicant_code", value = "")
      updateTextInput(session, "school1", value = "")
      updateTextInput(session, "school2", value = "")
      updateTextInput(session, "school3", value = "")
      updateTextInput(session, "school4", value = "")
      updateTextInput(session, "school5", value = "")
      updateTextInput(session, "AdminProv", value = "")
      updateTextInput(session, "AdminMun", value = "")
      updateTextInput(session, "AdminLevel", value = "")
      updateTextInput(session, "AdminRegion", value = "")
    }
  })
  # output$some_output_for_user1 <- renderText({"Content for user1..."})
  
  
  
  output$STRIDE3 <- renderUI({
    # Custom styling
    tags$div(
      style = "width: 100%;",
    page_navbar(title = div(
      tags$span(
        strong("SPIMS Application Portal"),
        style = "font-family: 'Poppins'; font-size: 1em; margin-bottom: 0.2em;"),"",
    ),
    nav_spacer(),
    nav_panel(strong("School Preference Selection"), fluidRow(
      layout_columns(
        card(
          card_header(
            div(strong("Applicant School Preferences"), style = "font-family: 'Poppins'; font-size: 30px; color: #111111; padding: 1px; text-align: center;"))
        ),
        card(
          card_header(
            div(
              strong("Step 1: Enter Applicant Code"),
              br(),
              em("Please enter your applicant code. Your name, province, and level will be automatically filled.")
            )
          ),
          card_body(
            fluidRow(
              column(4,
                     textInput("applicant_code", "Applicant Control Code:*", placeholder = "ex: 000000"),
              ),
              column(8,)
            ),
            fluidRow(
              column(4,
                     textInputRO("user_name", "Name:*")),
              column(4,
                     textInputRO("user_level","Level:*")),
              column(4,
                     textInputRO("user_prov", "Province:*"))
            ),
          )
        ),
        card(
          card_header(
            div(
              strong("Step 2: Enter Preferred Municiaplity"),
              br(),
              em("Select your preferred municipality. You may also select up to 2 additional municipalities near you, but this is not required."))
          ),
          card_body(
            fluidRow(
              column(4,
                     uiOutput("MunSelection1")
              ),
              column(4,
                     uiOutput("MunSelection2")
              ),
              column(4,
                     uiOutput("MunSelection3")
              )
            ),
            actionButton("munconfirm", strong("Confirm Municipality Selection"), class = "btn-success")
          )
        ),
        
        card(height = 700,
             card_header(
               div(
                 strong("Step 3: Select School from the map or the dropdown",
                        br(),
                        em("You must fill out all required fields to view schools."))
               )
             ),
             fluidRow(
               column(width=5, 
                      selectInput("school1", "First Preferred School:", choices = c("")),
                      selectInput("school2", "Second Preferred School:", choices = c("")),
                      selectInput("school3", "Third Preferred School:", choices = c("")),
                      selectInput("school4", "Fourth Preferred School:", choices = c("")),
                      selectInput("school5", "Fifth Preferred School:", choices = c("")),
                      actionButton("clear_selections", "Clear All Selections", class = "btn-danger")
               ),
               column(width=7,
                      leafletOutput("SPIMSMapping", height = "590px")
               )
             )
             
        ),
        card(height = 75,
             actionButton("submitBtn", strong("Submit Preferences"), class = "btn-success")
        ),
        #card(height = 300,
        #      uiOutput("survey_check")
        
        #),
        col_widths = c(12,12,12,12,12)))),
    
    nav_panel(strong("Frequently Asked Questions"), #We should also add groupings for the questions to make it easier to navigate
              fluidRow(
                layout_columns(
                  col_widths = c(12),  # Single full-width column for FAQ title
                  card(
                    card_header(
                      div(
                        strong("Frequently Asked Questions"),
                        style = "font-family: Century Gothic; font-size: 30px; color: #111111; padding: 1px; text-align: center;"
                      )
                    )
                  )
                )
              ),
              fluidRow(
                layout_columns(
                  width = 12,
                  tags$div(
                    class = "accordion", id = "faqAccordion",
                    
                    # Q1
                    tags$div(class = "accordion-item",
                             tags$h2(class = "accordion-header",
                                     tags$button(
                                       class = "accordion-button collapsed",
                                       type = "button",
                                       `data-bs-toggle` = "collapse",
                                       `data-bs-target` = "#faq1",
                                       strong("What will serve as my endorsement letter to the Schools Division Office (SDO)?")
                                     )
                             ),
                             tags$div(
                               id = "faq1",
                               class = "accordion-collapse collapse",
                               `data-bs-parent` = "#faqAccordion",
                               tags$div(
                                 class = "accordion-body",
                                 style = "font-family: Helvetica; font-size: 18px; color: #111111; text-align: left;",
                                 "An email will be sent to you, indicating the SDO assignment, initial process, and required documents for the hiring process. This email serves as the official endorsement letter to the Schools Division."
                               )
                             )
                    ),
                    
                    # Q2
                    tags$div(class = "accordion-item",
                             tags$h2(class = "accordion-header",
                                     tags$button(
                                       class = "accordion-button collapsed",
                                       type = "button",
                                       `data-bs-toggle` = "collapse",
                                       `data-bs-target` = "#faq2",
                                       strong("What are the application requirements I need to submit to the SDO?")
                                     )
                             ),
                             tags$div(
                               id = "faq2",
                               class = "accordion-collapse collapse",
                               `data-bs-parent` = "#faqAccordion",
                               tags$div(
                                 class = "accordion-body",
                                 style = "font-family: Helvetica; font-size: 18px; color: #111111; text-align: left;",
                                 tagList(
                                   "In reference to DepEd Order No. 007, s. 2023 and DepEd Order No. 021 s. 2024 the following are the required documents:",
                                   tags$ul(
                                     tags$li("Duly accomplished PDS (CS Form No. 212, Revised 2017) with Work Experience Sheet, if applicable;"),
                                     tags$li("Photocopy of valid and updated PRC License/ID;"),
                                     tags$li("Photocopy of scholastic/academic records such as but not limited to Transcript of Records (TOR) and Diploma;"),
                                     tags$li("Photocopy of Certificate/s of Training, if applicable;"),
                                     tags$li("Photocopy of Certificate of Employment, Contract of Service, or duly signed service record, whichever is/are applicable;"),
                                     tags$li("Other documents as may be required by the HRMPSB for comparative assessment.")
                                   ))
                               )
                             )
                    ),
                    # Q3
                    tags$div(class = "accordion-item",
                             tags$h2(class = "accordion-header",
                                     tags$button(
                                       class = "accordion-button collapsed",
                                       type = "button",
                                       `data-bs-toggle` = "collapse",
                                       `data-bs-target` = "#faq3",
                                       strong("Where should I submit the required documents?")
                                     )
                             ),
                             tags$div(
                               id = "faq3",
                               class = "accordion-collapse collapse",
                               `data-bs-parent` = "#faqAccordion",
                               tags$div(
                                 class = "accordion-body",
                                 style = "font-family: Helvetica; font-size: 18px; color: #111111; text-align: left;",
                                 "The documents/requirements should be submitted to your respective SDOs to facilitate the next steps of hiring and assessment according to the instructions set by the Division Office."
                               )
                             )
                    ),
                    # Q4
                    tags$div(class = "accordion-item",
                             tags$h2(class = "accordion-header",
                                     tags$button(
                                       class = "accordion-button collapsed",
                                       type = "button",
                                       `data-bs-toggle` = "collapse",
                                       `data-bs-target` = "#faq4",
                                       strong("What will be my next step after I submit my initial documents to the SDO?")
                                     )
                             ),
                             tags$div(
                               id = "faq4",
                               class = "accordion-collapse collapse",
                               `data-bs-parent` = "#faqAccordion",
                               tags$div(
                                 class = "accordion-body",
                                 style = "font-family: Helvetica; font-size: 18px; color: #111111; text-align: left;",
                                 "After submitting your documents, you will need to wait for the confirmation of your SDO, as well as the information on the set timeline for the hiring and assessment process."
                               )
                             )
                    ),
                    # Q5
                    tags$div(class = "accordion-item",
                             tags$h2(class = "accordion-header",
                                     tags$button(
                                       class = "accordion-button collapsed",
                                       type = "button",
                                       `data-bs-toggle` = "collapse",
                                       `data-bs-target` = "#faq5",
                                       strong("Do I need to undergo the regular hiring process?")
                                     )
                             ),
                             tags$div(
                               id = "faq5",
                               class = "accordion-collapse collapse",
                               `data-bs-parent` = "#faqAccordion",
                               tags$div(
                                 class = "accordion-body",
                                 style = "font-family: Helvetica; font-size: 18px; color: #111111; text-align: left;",
                                 "You are required to undergo the regular hiring process. Regardless of the assessment score obtained, you will still be hired and deployed in DepEd."
                               )
                             )
                    ),
                    # Q6
                    tags$div(class = "accordion-item",
                             tags$h2(class = "accordion-header",
                                     tags$button(
                                       class = "accordion-button collapsed",
                                       type = "button",
                                       `data-bs-toggle` = "collapse",
                                       `data-bs-target` = "#faq6",
                                       strong("Do I still qualify for priority hiring?")
                                     )
                             ),
                             tags$div(
                               id = "faq6",
                               class = "accordion-collapse collapse",
                               `data-bs-parent` = "#faqAccordion",
                               tags$div(
                                 class = "accordion-body",
                                 style = "font-family: Helvetica; font-size: 18px; color: #111111; text-align: left;",
                                 "Yes. As a beneficiary, you are a priority over regular applicants for hiring and deployment for a Teacher I Plantilla Item. "
                               )
                             )
                    ),
                    # Q7
                    tags$div(class = "accordion-item",
                             tags$h2(class = "accordion-header",
                                     tags$button(
                                       class = "accordion-button collapsed",
                                       type = "button",
                                       `data-bs-toggle` = "collapse",
                                       `data-bs-target` = "#faq7",
                                       strong("What alternative modes of assessment can be applied to me if I am still abroad?")
                                     )
                             ),
                             tags$div(
                               id = "faq7",
                               class = "accordion-collapse collapse",
                               `data-bs-parent` = "#faqAccordion",
                               tags$div(
                                 class = "accordion-body",
                                 style = "font-family: Helvetica; font-size: 18px; color: #111111; text-align: left;",
                                 "You may be assessed through alternative modes, such as online interviews, virtual assessments, or submitting required documents electronically subject to the arrangement of SDO."
                               )
                             )
                    ),
                    # Q8
                    tags$div(class = "accordion-item",
                             tags$h2(class = "accordion-header",
                                     tags$button(
                                       class = "accordion-button collapsed",
                                       type = "button",
                                       `data-bs-toggle` = "collapse",
                                       `data-bs-target` = "#faq8",
                                       strong("If I fail to undergo the assessment process, can I still be accommodated?")
                                     )
                             ),
                             tags$div(
                               id = "faq8",
                               class = "accordion-collapse collapse",
                               `data-bs-parent` = "#faqAccordion",
                               tags$div(
                                 class = "accordion-body",
                                 style = "font-family: Helvetica; font-size: 18px; color: #111111; text-align: left;",
                                 "No. It is required for you to undergo the assessment process as stipulated in DO No. 007, s. 2023 and DO No. 021, s. 2024."
                               )
                             )
                    ),
                    # Q9
                    tags$div(class = "accordion-item",
                             tags$h2(class = "accordion-header",
                                     tags$button(
                                       class = "accordion-button collapsed",
                                       type = "button",
                                       `data-bs-toggle` = "collapse",
                                       `data-bs-target` = "#faq9",
                                       strong("What happens if I fail to report to the SDO until August 29, 2025?")
                                     )
                             ),
                             tags$div(
                               id = "faq9",
                               class = "accordion-collapse collapse",
                               `data-bs-parent` = "#faqAccordion",
                               tags$div(
                                 class = "accordion-body",
                                 style = "font-family: Helvetica; font-size: 18px; color: #111111; text-align: left;",
                                 "The allotted teaching item for you shall be deemed as WAIVED. As a consequence, you will not be allowed to re-apply for the SPIMS Program."
                               )
                             )
                    ),
                    # Q10
                    tags$div(class = "accordion-item",
                             tags$h2(class = "accordion-header",
                                     tags$button(
                                       class = "accordion-button collapsed",
                                       type = "button",
                                       `data-bs-toggle` = "collapse",
                                       `data-bs-target` = "#faq10",
                                       strong("What happens if the SDO is unable to contact me?")
                                     )
                             ),
                             tags$div(
                               id = "faq10",
                               class = "accordion-collapse collapse",
                               `data-bs-parent` = "#faqAccordion",
                               tags$div(
                                 class = "accordion-body",
                                 style = "font-family: Helvetica; font-size: 18px; color: #111111; text-align: left;",
                                 "The SDO will exhaust all means to contact you. But you are also responsible to coordinate diligently with the SDO. However, if the SDO is unable to establish communication with you, the teaching item allotted for you shall be deemed waived."
                               )
                             )
                    ),
                    # Q11
                    tags$div(class = "accordion-item",
                             tags$h2(class = "accordion-header",
                                     tags$button(
                                       class = "accordion-button collapsed",
                                       type = "button",
                                       `data-bs-toggle` = "collapse",
                                       `data-bs-target` = "#faq11",
                                       strong("Can I re-apply if I waive or refuse my appointment?")
                                     )
                             ),
                             tags$div(
                               id = "faq11",
                               class = "accordion-collapse collapse",
                               `data-bs-parent` = "#faqAccordion",
                               tags$div(
                                 class = "accordion-body",
                                 style = "font-family: Helvetica; font-size: 18px; color: #111111; text-align: left;",
                                 "No. The SPIMS Program does not allow for a re-application once you waive your privilege as a beneficiary. However, should you later decide to apply as a DepEd teacher, you will need to go through the regular hiring process without priority hiring."
                               )
                             )
                    ),
                    # Q12
                    tags$div(class = "accordion-item",
                             tags$h2(class = "accordion-header",
                                     tags$button(
                                       class = "accordion-button collapsed",
                                       type = "button",
                                       `data-bs-toggle` = "collapse",
                                       `data-bs-target` = "#faq12",
                                       strong("As a beneficiary, can I request a change in school assignment?")
                                     )
                             ),
                             tags$div(
                               id = "faq12",
                               class = "accordion-collapse collapse",
                               `data-bs-parent` = "#faqAccordion",
                               tags$div(
                                 class = "accordion-body",
                                 style = "font-family: Helvetica; font-size: 18px; color: #111111; text-align: left;",
                                 "Yes. You can request a change in school assignment following the provided matrix.",
                                 tags$img(
                                   src = "schoolchange.jpg",  # Replace with actual image path
                                   style = "max-width: 100%; height: auto; margin-bottom: 15px;"
                                 )
                               )
                             )
                    ),
                    # Q13
                    tags$div(class = "accordion-item",
                             tags$h2(class = "accordion-header",
                                     tags$button(
                                       class = "accordion-button collapsed",
                                       type = "button",
                                       `data-bs-toggle` = "collapse",
                                       `data-bs-target` = "#faq13",
                                       strong("Which Office should I contact regarding Teaching Kit concerns?")
                                     )
                             ),
                             tags$div(
                               id = "faq13",
                               class = "accordion-collapse collapse",
                               `data-bs-parent` = "#faqAccordion",
                               tags$div(
                                 class = "accordion-body",
                                 style = "font-family: Helvetica; font-size: 18px; color: #111111; text-align: left;",
                                 HTML("You are advised to coordinate with Department of Migrant Workers (DMW). Below is DMW’s contact information:

<br/>Facebook Pages: SPIMS NRCO and NRCO Central
<br/>Telephone Number: (02) 8722-11-61
<br/>Email Address: spims@dmw.gov.ph")
                               )
                             )
                    ),
                    col_widths = c(-3,6,3,-1,10,-1,-1,10,-1,-1,10,-1))))),
    
    nav_panel(strong("Contact Us"),
              fluidRow(
                layout_columns(
                  HTML('<img src="Contactus.png" width="100%" height="auto">')
                )
              )
    ),
    nav_panel(strong("Submit a Query"),
              fluidRow(
                column(
                  width = 12,
                  tags$iframe(
                    src = "https://docs.google.com/forms/d/e/1FAIpQLSdIAPyNIOffLuGI7UHIV4-5IxglQozQAE0NvD2R5a3qX4NOBA/viewform?embedded=true",
                    width = "100%",
                    height = "800",
                    frameborder = "0",
                    marginheight = "0",
                    marginwidth = "0"
                  )
                )
              )
    ),
    nav_item(
      tags$div(
        style = "display: flex; align-items: center; margin-right: 10px;",
        shinyauthr::logoutUI(
          id = "logout",
          label = "Log Out",
          class = "btn btn-danger btn-sm"
        )
      )
    )
    )
    )# Custom styling
  })
  
  
  output$STRIDE4 <- renderUI({
    # Custom styling
    page_navbar(title = div(
      tags$span(
        strong("SPIMS Application Admin Portal"),
        style = "font-size: 1em; margin-bottom: 0.2em;"),"",
    ),
    nav_spacer(),
    nav_panel(
      strong("School Preference Submissions"),
      card(
        card_header(
          div(strong("Summary Statistics"), style = "font-family: Century Gothic; font-size: 30px; color: #111111; padding: 1px; text-align: center;")
        )),
      
      fluidRow(
        layout_columns(
          col_widths = c(4),
          card(
            card_header(strong("# Applicant Preferences Submitted"), style = "text-align: center;"),
            card_body(
              valueBoxOutput("submission_number")
            )
          ),
          card(
            card_header(strong("Total # of SPIMS Applicants"), style = "text-align: center;"),
            card_body(
              valueBoxOutput("total_number")
            )
          ),
          card(
            card_header(strong("Preference Submission Rate"), style = "text-align: center;"),
            card_body(
              valueBoxOutput("percent_number")
            )
          )
        )
      ),
      card(
        card_header(
          div(strong("Applicant School Preferences"), style = "font-family: Century Gothic; font-size: 30px; color: #111111; padding: 1px; text-align: center;")),
        card_body(
          DT::DTOutput("dataTable")
        )),
      card(
        card_header(
          div(strong("School Preference Counts"), style = "font-family: Century Gothic; font-size: 30px; color: #111111; padding: 1px; text-align: center;")
        ),
        card_body(
          DT::DTOutput("school_count_table")
        )
      )
    ),
    nav_panel(strong("Map Visualization"),fluidRow(
      layout_sidebar(
        sidebar = sidebar(
          class = "bg-secondary",
          h6("Filters:"),
          selectInput("AdminRegion","Select a Region:", multiple = FALSE, c(" " = " ","-" = "-","Region I" = "Region I","Region II" = "Region II","Region III" = "Region III","Region IV-A" = "Region IV-A","Region IV-B" = "Region IV-B","Region V" = "Region V","Region VI" = "Region VI","Region VII" = "Region VII","Region VIII" = "Region VIII","Region IX" = "Region IX","Region X" = "Region X","Region XI" = "Region XI","Region XII" = "Region XII","CARAGA" = "CARAGA","CAR" = "CAR","NIR" = "NIR","NCR" = "NCR")),
          uiOutput("AdminProvSelection"),
          uiOutput("AdminMunSelection"),
          selectInput("AdminLevel","Select a level:", multiple = FALSE, c(" "=" ","Elementary School"="Elementary School","Junior High School"="Junior High School")),
          input_task_button("AdminRun", strong("Show Selection"), class = "btn-success"),
          input_task_button("AdminClearSelection", strong("Clear Selection"), class = "btn-success")
        ),
        layout_columns(
          card(height = 500,
               card_header(strong("Applicant Preference Mapping")),
               leafletOutput("AdminMapping")),
        )
      )
    )),
    nav_item(
      tags$div(
        style = "display: flex; align-items: center; margin-right: 10px;",
        shinyauthr::logoutUI(
          id = "logout",
          label = "Log Out",
          class = "btn btn-danger btn-sm"
        )
      )
    )
    )
  })
  
  
  # Clean and prepare data
  school <- school %>%
    mutate(
      Latitude = as.numeric(Latitude),
      Longitude = as.numeric(Longitude),
      ES.Shortage = as.numeric(ES.Shortage),
      JHS.Shortage = as.numeric(JHS.Shortage),
      SHA.2024.Index = as.numeric(SHA.2024.Index),
      SHA_Category = case_when(
        SHA.2024.Index > 0.33 ~ "Yes",
        TRUE ~ "No"
      ),
      #Renamed these to look better
      Province = ifelse(Province == "MANILA, NCR,  FIRST DISTRICT ", "NCR 1ST DISTRICT", Province),
      Province = ifelse(Province == "NCR   THIRD DISTRICT", "NCR 3RD DISTRICT", Province),
      Province = ifelse(Province == "NCR   SECOND DISTRICT", "NCR 2ND DISTRICT", Province),
      Province = ifelse(Province == "NCR   FOURTH DISTRICT", "NCR 4TH DISTRICT", Province)
    )
  get_school_name <- function(id) {
    school %>%
      filter(SchoolID == id) %>%
      pull(SchoolName) %>%
      first()
  }
  #Output and Reactive Lines
  
  #Municipality Selection
  observe({
    all_choices <- sort(school[school$Province == input$user_prov, "Municipality"])
    
    updateSelectInput(session, "user_mun", choices = all_choices, selected = input$user_mun)
    
    remaining_for_2 <- setdiff(all_choices, input$user_mun)
    updateSelectInput(session, "user_mun_2", choices = remaining_for_2, selected = input$user_mun_2)
    
    remaining_for_3 <- setdiff(remaining_for_2, input$user_mun_2)
    updateSelectInput(session, "user_mun_3", choices = remaining_for_3, selected = input$user_mun_3)
  })
  
  # Render UI inputs for Municipality Selection
  output$MunSelection1 <- renderUI({
    selectInput("user_mun", HTML('Select your <em>preferred</em> Municipality:*'), choices = NULL)
  })
  output$MunSelection2 <- renderUI({
    selectInput("user_mun_2","Select an additional Municipality:", choices = NULL)
  })
  output$MunSelection3 <- renderUI({
    selectInput("user_mun_3","Select an additional Municipality:", choices = NULL)
  })
  
  output$SPIMSMapping <- renderLeaflet({
    p = colorFactor(palette = c("red","orange", "green"),domain = c("Most Competitive","Moderately Competitive", "Least Competitive"), ordered = T)
    leaflet() %>%
      setView(lng = 122, lat = 12, zoom = 5) %>%
      addProviderTiles(providers$Esri.WorldImagery, group = "Satellite") %>%
      addProviderTiles(providers$CartoDB.Positron, group = "Road Map") %>%
      addLegend(position = "bottomright", title = "Location Availability", pal = p, values = c("Most Competitive", "Moderately Competitive", "Least Competitive")) %>%
      addLayersControl(
        baseGroups = c("Satellite", "Road Map")
      ) %>%
      addEasyButton( #Allows a person viewing in browser to add their location on a map. I haven't tested if it works on something like an iPhone yet.
        easyButton(
          icon = "fa-crosshairs",
          title = "Show My Location",
          onClick = JS("
          function(btn, map) {
            map.locate({setView: true, maxZoom: 15});
            map.on('locationfound', function(e) {
              if (typeof window.userLocationMarker !== 'undefined') {
                map.removeLayer(window.userLocationMarker);
              }
              window.userLocationMarker = L.circleMarker(e.latlng, {
                radius: 6,
                color: '#007BFF',
                fillColor: '#007BFF',
                fillOpacity: 0.8
              }).addTo(map).bindPopup('You are here').openPopup();
              window.userLocationCircle = L.circle(e.latlng, {
                radius: 300,
                color: '#007BFF',
                fillColor: '#007BFF',
                fillOpacity: 0.2,
                weight: 1
              }).addTo(map);
            });
          }
        ")
        )
      )
  })
  #Clearing the values of the selected schools
  selectedSchoolsES <- reactiveVal(character(0))
  selectedSchoolsJHS <- reactiveVal(character(0))
  mainreactsch <- reactiveVal(data.frame())
  
  getSelectedSchools <- function() {
    if (input$user_level == "Elementary School") {
      selectedSchoolsES()
    } else if (input$user_level == "Junior High School") {
      selectedSchoolsJHS()
    } else {
      NULL
    }
  }
  
  setSelectedSchools <- function(value) {
    valid_ids <- mainreactsch()$SchoolName
    value <- intersect(value, valid_ids)
    
    if (input$user_level == "Elementary School") {
      selectedSchoolsES(value)
    } else if (input$user_level == "Junior High School") {
      selectedSchoolsJHS(value)
    }
  }
  
  
  observeEvent(input$applicant_code, {
    req(input$applicant_code)
    
    conn <- dbConnect(RSQLite::SQLite(), db_path)
    on.exit(dbDisconnect(conn), add = TRUE)
    
    result <- dbGetQuery(conn, "SELECT COUNT(*) as n FROM entries WHERE applicant_code = ?", 
                         params = list(input$applicant_code))
    
    if (result$n > 0) {
      # Applicant code already made submission
      shinyalert(
        title = "Preferences Already Submitted",
        text = "This applicant code already has preferences recorded. Check the code again. If your information is correct, please contact the SPIMS team.",
        type = "error"
      )
      return()
    }
    
    # Find the row that matches the applicant_code
    matched <- user_data[user_data$applicant_code == input$applicant_code, ]
    
    if (nrow(matched) == 1) {
      updateTextInput(session, "user_prov", value = matched$Province)
      updateTextInput(session, "user_level", value = matched$Level)
      updateTextInput(session, "user_name", value = matched$Name)
    } else {
      # Clear the fields if no match is found
      updateTextInput(session, "user_prov", value = "")
      updateTextInput(session, "user_level", value = "")
      updateTextInput(session, "user_name", value = "")
    }
    leafletProxy("SPIMSMapping") %>%
      clearGroup("highlight") %>%
      clearGroup("base") %>%
      clearMarkers()
  })
  
  observeEvent(input$munconfirm, {
    
    if (is.null(input$applicant_code) || input$applicant_code == "" ||
        is.null(input$user_name) || input$user_name == "" ||
        is.null(input$user_prov) || input$user_prov == "" ||
        is.null(input$user_mun) || input$user_mun == "" ||
        is.null(input$user_level) || input$user_level == "") {
      shinyalert(
        title = "Incomplete Fields",
        text = "Please fill in all required fields before submitting. You must select at least one school.",
        type = "error"
      )
      return()
    }
    selectedSchoolsES(character(0))
    selectedSchoolsJHS(character(0))
    setSelectedSchools(character(0))
    mainreactsch(data.frame())
    
    ProvRCT <- input$user_prov
    MunRCT <- input$user_mun
    MunRCT2 <- input$user_mun_2
    MunRCT3 <- input$user_mun_3
    LevelRCT <- input$user_level
    
    data <- school %>%
      filter(Province == ProvRCT, 
             Municipality %in% c(MunRCT, MunRCT2, MunRCT3)) %>%
      filter(ES.Shortage > 0 | JHS.Shortage > 0) %>%
      arrange(SchoolName)
    
    if (LevelRCT == "Elementary School") {
      data <- data %>% filter(!is.na(ES.Shortage) & ES.Shortage > 0)
    } else if (LevelRCT == "Junior High School") {
      data <- data %>% filter(!is.na(JHS.Shortage) & JHS.Shortage > 0)
    }
    
    #Merging the mainreactsch values with the constantly updating number of applicants in school_counts
    conn <- get_db_conn()
    counts_df <- dbReadTable(conn, "school_counts")
    counts_df <- counts_df %>% rename("SchoolID" = school_id) %>% mutate(SchoolID = as.integer(SchoolID))
    dbDisconnect(conn)
    
    data <- left_join(data, counts_df, by = "SchoolID") %>%
      mutate(number_applicants = ifelse(is.na(number_applicants), 0, number_applicants))
    
    mainreactsch(data)
    
    shortage_col <- if (LevelRCT == "Elementary School") {
      "ES.Shortage"
    } else {
      "JHS.Shortage"
    }
    
    mainreactsch(mainreactsch() %>%
                   mutate(
                     number_applicants = ifelse(is.na(number_applicants), 0, number_applicants),
                     shortage = .data[[shortage_col]],
                     diff = shortage - first_pref_schools,
                     ratio = if_else(number_applicants == 0, shortage, shortage / number_applicants),
                     markerColor = case_when(
                       ratio <= 1 ~ "red",                  # Fully filled or over-applied
                       ratio <= 5 ~ "orange",              # Moderate competition
                       ratio > 5 ~ "green",                # Under-applied
                       TRUE ~ "gray"                       # Fallback
                     )
                   )
    )
    
    if (nrow(data) == 0) {
      shinyalert(
        title = "No Schools Found",
        text = "No schools with a teacher shortage were found for your selected municipality. Please try another selection with a municipality nearby.",
        type = "error"
      )
      return()  # Stop the observer here
    }
    
    province_coords <- mainreactsch() %>%
      group_by(Municipality) %>%
      summarize(
        Mun_Latitude = mean(Latitude, na.rm = TRUE),
        Mun_Longitude = mean(Longitude, na.rm = TRUE)
      )
    
    coords <- province_coords %>% filter(Municipality %in% c(MunRCT, MunRCT2, MunRCT3))
    
    labels <- paste0(
      "School: ", mainreactsch()$SchoolName,
      "<br/> Teacher Shortage: ", if (LevelRCT == "Elementary School") mainreactsch()$ES.Shortage else mainreactsch()$JHS.Shortage,
      "<br/> SPIMS Applicants: ", mainreactsch()$number_applicants,
      "<br/> Special Hardship Allowance: ", mainreactsch()$SHA_Category
    ) %>% lapply(htmltools::HTML)
    
    leafletProxy("SPIMSMapping") %>%
      clearGroup("highlight") %>%
      clearGroup("base") %>%
      clearMarkers() %>%
      addProviderTiles(providers$Esri.WorldImagery, group = "Satellite") %>%
      addProviderTiles(providers$CartoDB.Positron, group = "Road Map") %>%
      setView(lng = coords$Mun_Longitude[1], lat = coords$Mun_Latitude[1], zoom = 11) %>%
      addAwesomeMarkers(
        data = mainreactsch(),
        lng = ~Longitude,
        lat = ~Latitude,
        icon = awesomeIcons(
          icon = 'education',
          library = 'glyphicon',
          markerColor = mainreactsch()$markerColor),
        layerId = ~SchoolID,
        group = "base",
        label = lapply(labels, htmltools::HTML))
  })
  
  observeEvent(input$SPIMSMapping_marker_click, {
    clicked_school <- input$SPIMSMapping_marker_click$id
    req(clicked_school)
    
    isolate({
      current <- c(input$school1, input$school2, input$school3, input$school4, input$school5)
      current[current == ""] <- NA
      
      if (clicked_school %in% current) {
        if (clicked_school == input$school1) {
          updateSelectInput(session, "school1", selected = "")
        } else if (clicked_school == input$school2) {
          updateSelectInput(session, "school2", selected = "")
        } else if (clicked_school == input$school3) {
          updateSelectInput(session, "school3", selected = "")
        } else if (clicked_school == input$school4) {
          updateSelectInput(session, "school4", selected = "")
        } else if (clicked_school == input$school5) {
          updateSelectInput(session, "school5", selected = "")
        }
      } else {
        first_empty <- which(is.na(current))[1]
        if (!is.na(first_empty)&& first_empty <= 5) {
          updateSelectInput(session, paste0("school", first_empty), selected = clicked_school)
        }
      }
    })
  })
  
  observe({
    data <- mainreactsch()
    req(data)
    req("SchoolName" %in% names(data))
    
    selected <- c(input$school1, input$school2, input$school3, input$school4, input$school5)
    selected <- selected[selected != ""]
    
    # Dynamically override color
    map_data <- data %>%
      mutate(
        baseColor = case_when(
          ratio <= 1 ~ "red",                  # Fully filled or over-applied
          ratio <= 5 ~ "orange",              # Moderate competition
          ratio > 5 ~ "green",                # Under-applied
          TRUE ~ "gray"                       # Fallback
        ),
        markerColor = ifelse(SchoolID %in% selected, "blue", baseColor)
      )
    
    labels <- paste0(
      "School: ", map_data$SchoolName,
      "<br/> Teacher Shortage: ", if (input$user_level == "Elementary School") map_data$ES.Shortage else map_data$JHS.Shortage,
      "<br/> SPIMS Applicants: ", map_data$number_applicants,
      "<br/> Special Hardship Allowance: ", map_data$SHA_Category
    ) %>% lapply(htmltools::HTML)
    
    leafletProxy("SPIMSMapping") %>%
      clearMarkers() %>%
      addProviderTiles(providers$Esri.WorldImagery, group = "Satellite") %>%
      addProviderTiles(providers$CartoDB.Positron, group = "Road Map") %>%
      addAwesomeMarkers(
        data = map_data,
        lng = ~Longitude,
        lat = ~Latitude,
        icon = awesomeIcons(
          icon = 'education',
          library = 'glyphicon',
          markerColor = ~markerColor
        ),
        layerId = ~SchoolID,
        label = labels,
        group = "base"
      )
  })
  
  observeEvent(input$clear_selections, {
    # Clear reactiveVals
    selectedSchoolsES(character(0))
    selectedSchoolsJHS(character(0))
    setSelectedSchools(character(0))
    
    # Clear dropdown inputs
    updateSelectInput(session, "school1", selected = "")
    updateSelectInput(session, "school2", selected = "")
    updateSelectInput(session, "school3", selected = "")
    updateSelectInput(session, "school4", selected = "")
    updateSelectInput(session, "school5", selected = "")
    
    data <- mainreactsch()
    req(data)
    
    map_data <- data %>%
      mutate(
        baseColor = case_when(
          ratio <= 1 ~ "red",                  # Fully filled or over-applied
          ratio <= 5 ~ "orange",              # Moderate competition
          ratio > 5 ~ "green",                # Under-applied
          TRUE ~ "gray"                       # Fallback
        ),
        markerColor = baseColor
      )
    
    labels <- paste0(
      "School: ", map_data$SchoolName,
      "<br/> Teacher Shortage: ", if (input$user_level == "Elementary School") map_data$ES.Shortage else map_data$JHS.Shortage,
      "<br/> SPIMS Applicants: ", map_data$number_applicants,
      "<br/> Special Hardship Allowance: ", map_data$SHA_Category
    ) %>% lapply(htmltools::HTML)
    
    leafletProxy("SPIMSMapping") %>%
      clearMarkers() %>%
      addAwesomeMarkers(
        data = map_data,
        lng = ~Longitude,
        lat = ~Latitude,
        icon = awesomeIcons(
          icon = 'education',
          library = 'glyphicon',
          markerColor = ~markerColor
        ),
        layerId = ~SchoolID,
        label = labels,
        group = "base"
      )
  })
  
  #SURVEY FUNCTIONALITY
  # UI Rendering
  #output$survey_check <- renderUI({
  #  tagList(
  #    h5("Recent Submissions:"),
  #    tableOutput("dataTable")
  #  )
  #})
  
  # Dynamic selectInput updates based on school selections - removes schools that have already been selected
  # Part 1: Update 'school2' when 'school1' changes
  # 1. This observe block runs once when the app starts
  #    and whenever the main school data changes. It populates
  #    the initial choices for the 'school1' dropdown.
  # 1. Populates 'school1' on app startup.
  # 1. Populates 'school1' on app startup.
  observe({
    school_data <- mainreactsch()
    
    req(nrow(school_data) > 0)
    
    choices_named <- setNames(school_data$SchoolID, school_data$SchoolName)
    
    updateSelectInput(session, "school1", choices = choices_named, selected = "")
  })
  
  # 2. Updates 'school2' when 'school1' changes.
  observeEvent(input$school1, {
    school_data <- mainreactsch()
    
    req(nrow(school_data) > 0, input$school1)
    
    selected_school_id <- input$school1
    
    filtered_data_2 <- school_data %>%
      filter(SchoolID != selected_school_id)
    
    remaining_for_2 <- setNames(filtered_data_2$SchoolID, filtered_data_2$SchoolName)
    
    # Use an empty string to ensure nothing is selected
    updateSelectInput(session, "school2", choices = remaining_for_2, selected = "")
  })
  
  # 3. Updates 'school3' when 'school2' changes.
  observeEvent(input$school2, {
    school_data <- mainreactsch()
    
    req(nrow(school_data) > 0, input$school1, input$school2)
    
    selected_school1_id <- input$school1
    selected_school2_id <- input$school2
    
    filtered_data_3 <- school_data %>%
      filter(SchoolID != selected_school1_id, SchoolID != selected_school2_id)
    
    remaining_for_3 <- setNames(filtered_data_3$SchoolID, filtered_data_3$SchoolName)
    
    # Use an empty string to ensure nothing is selected
    updateSelectInput(session, "school3", choices = remaining_for_3, selected = "")
  })
  
  # 4. NEW: Updates 'school4' when 'school3' changes.
  observeEvent(input$school3, {
    school_data <- mainreactsch()
    req(nrow(school_data) > 0, input$school1, input$school2, input$school3)
    
    selected_schools <- c(input$school1, input$school2, input$school3)
    
    filtered_data_4 <- school_data %>%
      filter(!SchoolID %in% selected_schools)
    
    remaining_for_4 <- setNames(filtered_data_4$SchoolID, filtered_data_4$SchoolName)
    
    updateSelectInput(session, "school4", choices = remaining_for_4, selected = "")
  })
  
  # 5. NEW: Updates 'school5' when 'school4' changes.
  observeEvent(input$school4, {
    school_data <- mainreactsch()
    req(nrow(school_data) > 0, input$school1, input$school2, input$school3, input$school4)
    
    selected_schools <- c(input$school1, input$school2, input$school3, input$school4)
    
    filtered_data_5 <- school_data %>%
      filter(!SchoolID %in% selected_schools)
    
    remaining_for_5 <- setNames(filtered_data_5$SchoolID, filtered_data_5$SchoolName)
    
    updateSelectInput(session, "school5", choices = remaining_for_5, selected = "")
  })
  
  # Render UI inputs
  selectInput("school1", "First Preferred School:", choices = NULL, selected = NULL)
  selectInput("school2", "Second Preferred School:", choices = NULL, selected = NULL)
  selectInput("school3", "Third Preferred School:", choices = NULL, selected = NULL)
  selectInput("school4", "Fourth Preferred School:", choices = NULL, selected = NULL)
  selectInput("school5", "Fifth Preferred School:", choices = NULL, selected = NULL)
  
  # DB connection helper
  get_db_conn <- function() {
    dbConnect(RSQLite::SQLite(), db_path)
  }
  # Submission logic
  observeEvent(input$submitBtn, {
    if (is.null(input$applicant_code) || input$applicant_code == "" ||
        is.null(input$user_name) || input$user_name == "" ||
        is.null(input$user_prov) || input$user_prov == "" ||
        is.null(input$user_mun) || input$user_mun == "" ||
        is.null(input$user_level) || input$user_level == "" || is.null(input$school1) || input$school1 == "") {
      shinyalert(
        title = "Incomplete Fields",
        text = "Please fill in all required fields before submitting. You must select at least one school.",
        type = "error"
      )
      return()
    } else {
      showModal(
        modalDialog(
          title = "Confirmation",
          tags$div(
            style = "font-family: Poppins; font-size: 20px; color: #111111;",
            tags$p(strong("Your Information")),
            tags$p(strong("Name: "), input$user_name),
            tags$p(strong("Applicant Code: "), input$applicant_code),
            tags$p(strong("School Preference 1: "), get_school_name(input$school1)),
            tags$p(strong("School Preference 2: "), get_school_name(input$school2)),
            tags$p(strong("School Preference 3: "), get_school_name(input$school3)),
            tags$p(strong("School Preference 4: "), get_school_name(input$school4)),
            tags$p(strong("School Preference 5: "), get_school_name(input$school5)),
            tags$hr(),
            "Are you sure you want to submit? You cannot change your choices once submitted."
          ),
          footer = tagList(
            actionButton("cancel_modal", "Cancel", class = "btn btn-danger"),
            actionButton("final_confirmation", "Submit", class = "btn btn-success")
          ),
          
          easyClose=FALSE
        )
      )
    }
  })
  
  observeEvent(input$cancel_modal, {
    removeModal()
  })
  
  observeEvent(input$final_confirmation, {
    if (nchar(trimws(input$user_name)) > 0) {
      tryCatch({
        conn <- get_db_conn()
        
        # Save submission with the correct column names
        dbExecute(conn, "INSERT INTO entries (user_name, applicant_code, school1_id, school2_id, school3_id, school4_id, school5_id, user_prov, user_mun, user_level)
                   VALUES (?, ?, ?, ?, ?, ?, ?, ?, ?, ?)",
                  params = list(input$user_name, input$applicant_code, input$school1, input$school2, input$school3, input$school4, input$school5, input$user_prov, input$user_mun, input$user_level))
        
        # Increment Counts
        chosen_schools <- c(input$school1, input$school2, input$school3, input$school4, input$school5)
        
        school_info <- school %>%
          filter(SchoolID %in% chosen_schools) %>%
          distinct(SchoolID, SchoolName, Municipality)
        
        for (i in seq_along(chosen_schools)) {
          sid <- chosen_schools[i]
          if (is.na(sid) || sid == "") next
          
          row <- school_info %>% filter(SchoolID == sid)
          school_id <- row$SchoolID[1]
          school_nm <- row$SchoolName[1]
          municipality <- row$Municipality[1]
          first_pref_increment <- ifelse(i == 1, 1, 0)
          
          dbExecute(conn, "
          INSERT INTO school_counts (school_id, school, municipality, number_applicants, first_pref_schools)
          VALUES (?, ?, ?, 1, ?)
          ON CONFLICT(school_id) DO UPDATE SET
            number_applicants = number_applicants + 1,
            first_pref_schools = first_pref_schools + excluded.first_pref_schools
        ", params = list(school_id, school_nm, municipality, first_pref_increment))
        }
        
        dbDisconnect(conn)
        
        #Prevent people from making multiple submissions
        shinyjs::disable("submitBtn")
        # UI feedback and reset
        removeModal()
        showModal(
          modalDialog(
            title = "Submit Success!",
            "Your submission has been received. You may now download your submission or proceed to log out",
            footer = downloadButton("download_pdf", "Download Confirmation PDF"),
            
            easyClose=TRUE
          )
        )
        
      })
    }
  })
  #Admin view
  spims_user_data_reactive <- reactive({
    input$submitBtn  # triggers refresh
    conn <- get_db_conn()
    data <- tryCatch({
      dbGetQuery(conn, "SELECT user_name, applicant_code, school1, school2, school3, school4, school5, user_prov, user_mun, user_level, timestamp FROM entries ORDER BY timestamp")
    }, error = function(e) {
      showNotification(paste("Error loading data:", e$message), type = "error")
      data.frame()
    }, finally = {
      if (dbIsValid(conn)) dbDisconnect(conn)
    })
    data
  })
  
  school_count_data_reactive <- reactive({
    conn <- get_db_conn()
    counts_df <- tryCatch({
      dbGetQuery(conn, "
      SELECT school_id, municipality, number_applicants, first_pref_schools
      FROM school_counts
    ")
    }, error = function(e) {
      showNotification(paste("Error loading school count data:", e$message), type = "error")
      data.frame()
    }, finally = {
      if (dbIsValid(conn)) dbDisconnect(conn)
    })
    
    # ADD THIS CONVERSION STEP ⬇️
    counts_df <- counts_df %>% 
      mutate(school_id = as.integer(school_id)) 
    
    # Join with the school dataset to get Region, Province, etc.
    full_data <- counts_df %>%
      left_join(school %>% select(SchoolID, SchoolName, Municipality, Province), 
                by = c("school_id" = "SchoolID"))
    
    full_data
  })
  
  output$dataTable <- DT::renderDataTable({
    DT::datatable(
      spims_user_data_reactive(),
      extensions = 'Buttons',
      options = list(
        dom = 'Bfrtip',
        buttons = c('copy', 'csv', 'excel', 'pdf', 'print'),
        pageLength = 10
      )
    )
  })
  
  output$school_count_table <- DT::renderDT({
    DT::datatable(
      school_count_data_reactive() %>%
        arrange(desc(first_pref_schools)) %>%
        select(SchoolID, Province, municipality, school, number_applicants, first_pref_schools),
      extensions = 'Buttons',
      options = list(
        dom = 'Bfrtip',
        buttons = c('copy', 'csv', 'excel', 'pdf', 'print'),
        pageLength = 10
      )
    )
  })
  
  #Value box output from number of submissons
  output$submission_number <- renderValueBox({
    # Connect to the SQLite database using the same db_path as init_db
    conn <- dbConnect(RSQLite::SQLite(), db_path)
    
    # Count number of rows in the 'entries' table
    submission_count <- dbGetQuery(conn, "SELECT COUNT(*) AS count FROM entries")$count
    
    # Close the connection
    dbDisconnect(conn)
    valueBox(
      tags$p(
        strong(submission_count),
        style = "font-family: Poppins; font-size: 20px; color: #111111; text-align: center;"
      ),
      subtitle = NULL
    )
  })
  
  output$total_number <- renderValueBox({
    number_of_applicants <- nrow(user_data)
    valueBox(
      tags$p(
        number_of_applicants,
        style = "font-family: Poppins; font-size: 20px; color: #111111; text-align: center;"
      ),
      subtitle = NULL
    )
  })
  
  output$percent_number <- renderValueBox({
    # Connect to the SQLite database using the same db_path as init_db
    conn <- dbConnect(RSQLite::SQLite(), db_path)
    
    # Count number of rows in the 'entries' table
    submission_count <- dbGetQuery(conn, "SELECT COUNT(*) AS count FROM entries")$count
    
    # Close the connection
    dbDisconnect(conn)
    number_of_applicants <- nrow(user_data)
    submitted_percent <- round((submission_count / number_of_applicants) * 100, 2)
    valueBox(
      tags$p(
        strong(submitted_percent, "%"),
        style = "font-family: Poppins; font-size: 20px; color: #111111; text-align: center;"
      ),
      subtitle = NULL
    )
  })
  
  # 1. Define the get_school_name function
  # This function is crucial for converting the SchoolID back to a SchoolName
  # It should be defined in your server logic, before your downloadHandler.
  output$download_pdf <- downloadHandler(
    filename = function() {
      code <- input$applicant_code
      if (is.null(code) || code == "") code <- "unknown"
      paste0("submission_", code, ".pdf")
    },
    content = function(file) {
      # Create temp directory and file paths
      temp_dir <- tempdir()
      temp_rmd <- file.path(temp_dir, "submission_report.Rmd")
      temp_pdf <- file.path(temp_dir, "output.pdf")  # Define BEFORE render
      
      # Copy Rmd file to temp directory
      file.copy("submission_report.Rmd", temp_rmd, overwrite = TRUE)
      
      # Try rendering to PDF
      tryCatch({
        # Get the full school data
        school_data <- mainreactsch()
        
        # A helper function to get the school name from an ID
        get_school_name <- function(school_id, data) {
          if (is.null(school_id) || school_id == "") {
            return(NA) # Return NA if the input is empty
          }
          
          # Find the row in the data with the matching SchoolID and return its SchoolName
          school_name <- data$SchoolName[data$SchoolID == school_id]
          
          if (length(school_name) == 0) {
            return(NA) # Return NA if no match is found
          }
          
          return(school_name)
        }
        
        # Retrieve the names for the selected IDs using the helper function
        school1_name <- get_school_name(input$school1, school_data)
        school2_name <- get_school_name(input$school2, school_data)
        school3_name <- get_school_name(input$school3, school_data)
        school4_name <- get_school_name(input$school4, school_data)
        school5_name <- get_school_name(input$school5, school_data)
        
        rmarkdown::render(
          input = temp_rmd,
          output_file = temp_pdf,
          output_format = "pdf_document",
          params = list(
            user_name = input$user_name,
            applicant_code = input$applicant_code,
            # Pass the new name variables to the R Markdown document
            school1 = school1_name,
            school2 = school2_name,
            school3 = school3_name,
            school4 = school4_name,
            school5 = school5_name
          ),
          envir = new.env(parent = globalenv())
        )
        
        # Only copy if render succeeded
        file.copy(temp_pdf, file, overwrite = TRUE)
        
      }, error = function(e) {
        showNotification(paste("PDF generation failed:", e$message), type = "error")
      })
    }
  )
  
  #Admin Mapping Functionality
  
  output$AdminProvSelection <- renderUI ({
    selectInput("AdminProv","Select your Province:", c(school[school$Region==input$AdminRegion,"Province"][order(school[school$Region == input$AdminRegion, "Province"])]))
  })
  
  output$AdminMunSelection <- renderUI({
    selectInput("AdminMun","Select your Municipality:", c(school[school$Province==input$AdminProv,"Municipality"][order(school[school$Province == input$AdminProv, "Municipality"])]))
  })
  
  output$AdminMapping <- renderLeaflet({
    p = colorFactor(palette = c("green","orange", "red"),domain = c("Least Competitive","Moderately Competitive", "Most Competitive"), ordered = T)
    leaflet() %>%
      setView(lng = 122, lat = 12, zoom = 5) %>%
      addProviderTiles(providers$Esri.WorldImagery, group = "Satellite") %>%
      addProviderTiles(providers$CartoDB.Positron, group = "Road Map") %>%
      addLegend(position = "bottomright", title = "School Shortage", pal = p, values = c("Least Competitive", "Moderately Competitive", "Most Competitive")) %>%
      addLayersControl(
        baseGroups = c("Satellite", "Road Map")
      )
  })
  
  observeEvent(input$AdminRun, {
    req(input$AdminRegion, input$AdminProv, input$AdminMun, input$AdminLevel)
    
    # Determine shortage column
    shortage_col <- if (input$AdminLevel == "Elementary School") {
      "ES.Shortage"
    } else {
      "JHS.Shortage"
    }
    
    # Filter school data
    filtered_schools <- school %>%
      filter(
        Region == input$AdminRegion,
        Province == input$AdminProv,
        Municipality == input$AdminMun
      ) %>%
      filter(.data[[shortage_col]] > 0)
    
    # Read number of applicants from database
    conn <- get_db_conn()
    counts_df <- dbReadTable(conn, "school_counts")
    dbDisconnect(conn)
    
    # Join and compute fullness
    data <- filtered_schools %>%
      left_join(counts_df, by = c("SchoolID" = "school_id")) %>%
      mutate(
        number_applicants = ifelse(is.na(number_applicants), 0, number_applicants),
        shortage = .data[[shortage_col]],
        diff = shortage - first_pref_schools,
        ratio = if_else(number_applicants == 0, shortage, shortage / number_applicants),
        markerColor = case_when(
          ratio <= 1 ~ "red",                  # Fully filled or over-applied
          ratio <= 5 ~ "orange",              # Moderate competition
          ratio > 5 ~ "green",                # Under-applied
          TRUE ~ "gray"                       # Fallback
        )
      )
    
    # Center map on municipality
    center_coords <- data %>%
      summarize(
        lon = mean(Longitude, na.rm = TRUE),
        lat = mean(Latitude, na.rm = TRUE)
      )
    
    # Build popup labels
    labels <- paste0(
      "School: ", data$SchoolName,
      "<br/> Teacher Shortage: ", data$shortage,
      "<br/> SPIMS Applicants: ", data$number_applicants,
      "<br/> First Preference Rankings: ", data$first_pref_schools,
      "<br/> Predicted Remaining Spots: ", data$diff,
      "<br/> Special Hardship Allowance: ", data$SHA_Category
      
    ) %>% lapply(htmltools::HTML)
    
    # Update leaflet map
    leafletProxy("AdminMapping") %>%
      clearGroup("highlight") %>%
      clearGroup("base") %>%
      clearMarkers() %>%
      setView(lng = center_coords$lon, lat = center_coords$lat, zoom = 10) %>%
      addAwesomeMarkers(
        data = data,
        lng = ~Longitude,
        lat = ~Latitude,
        icon = awesomeIcons(
          icon = 'education',
          library = 'glyphicon',
          markerColor = ~markerColor
        ),
        layerId = ~SchoolID,
        group = "base",
        label = labels
      )
  })
  
  
  
}

# Run the application 
shinyApp(ui = ui, server = server)