ui <- dashboardPage(
                
                    dashboardHeader(title = "Code Names"),
                    dashboardSidebar(width = 350,
                                     sidebarMenu(id = "tabs",
                                                 menuItem("Play Codenames Green", tabName = "overview", icon = icon("th"),
                                                          menuSubItem(text = "Choose game and side", "welcome"),
                                                          menuSubItem(text = "Side A", tabName = "chelle"),
                                                          menuSubItem(text = "Side B", tabName = "tom")
                                                 ),
                                                 menuItem("Add words",tabName =  "settings")
                                                 
                                                 ,
                                                 sidebarSearchForm(textId = "searchText", buttonId = "searchButton",
                                                                   label = "Search...")
                                     )
                    ),
                    dashboardBody(
                      tabItems(
                        tabItem(tabName = "welcome", helpText("Welcome"),
                                uiOutput('word_set_selection'),
                                
                                uiOutput('seed_text'),
                                 actionButton(inputId = "reset_seed",label = "Pick new number"),
                                helpText("Select a random number and make sure you have both selected the same number before choosing a side."),
                                helpText("The generated number is based on the time and will always be different."),
                                helpText("You and your opponent should only need to change the last two digits (or more if you are unlucky)."),
                                actionButton("side_a", label = "Side A"),
                                actionButton("side_b", label = "Side B"),
                                helpText("Please check with other players that the words are the same."),
                        ),
                        
                        tabItem(tabName = "chelle",
                                uiOutput("which_player"),
                                
                                  fluidRow(
                                           column(width = 2, plotOutput("plot_a", height = 95, click = "click_a")),
                                           column(width = 2, plotOutput("plot_b", height = 95, click = "click_b")),
                                           column(width = 2, plotOutput("plot_c", height = 95, click = "click_c")),
                                           column(width = 2, plotOutput("plot_d", height = 95, click = "click_d")),
                                           column(width = 2, plotOutput("plot_e", height = 95, click = "click_e"))
                                  ),
                                  fluidRow(
                                          column(width = 2, plotOutput("plot_f", height = 95, click = "click_f")),
                                          column(width = 2, plotOutput("plot_g", height = 95, click = "click_g")),
                                          column(width = 2, plotOutput("plot_h", height = 95, click = "click_h")),
                                          column(width = 2, plotOutput("plot_i", height = 95, click = "click_i")),
                                          column(width = 2, plotOutput("plot_j", height = 95, click = "click_j"))
                                  ),
                                fluidRow(
                                  column(width = 2, plotOutput("plot_k", height = 95, click = "click_k")),
                                  column(width = 2, plotOutput("plot_l", height = 95, click = "click_l")),
                                  column(width = 2, plotOutput("plot_m", height = 95, click = "click_m")),
                                  column(width = 2, plotOutput("plot_n", height = 95, click = "click_n")),
                                  column(width = 2, plotOutput("plot_o", height = 95, click = "click_o"))
                                ),
                                fluidRow(
                                  column(width = 2, plotOutput("plot_p", height = 95, click = "click_p")),
                                  column(width = 2, plotOutput("plot_q", height = 95, click = "click_q")),
                                  column(width = 2, plotOutput("plot_r", height = 95, click = "click_r")),
                                  column(width = 2, plotOutput("plot_s", height = 95, click = "click_s")),
                                  column(width = 2, plotOutput("plot_t", height = 95, click = "click_t"))
                                ),
                                fluidRow(
                                  column(width = 2, plotOutput("plot_u", height = 95, click = "click_u")),
                                  column(width = 2, plotOutput("plot_v", height = 95, click = "click_v")),
                                  column(width = 2, plotOutput("plot_w", height = 95, click = "click_w")),
                                  column(width = 2, plotOutput("plot_x", height = 95, click = "click_x")),
                                  column(width = 2, plotOutput("plot_y", height = 95, click = "click_y"))
                                ),
                               useShinyjs(),
                                uiOutput('done_button'),
                               fluidRow(
                                 column(width = 3, uiOutput('green_chelle')),
                                 tags$head(tags$style("#green_chelle{color: green;
                                 font-style: bold;
                                 }"
                                 )
                                 ),
                                 column(width = 3, uiOutput('black_chelle')),
                                 tags$head(tags$style("#black_chelle{color: black;
                                 font-style: bold;
                                 }"
                                 )
                                 ),
                                 column(width = 3, uiOutput('grey_chelle')),
                                 tags$head(tags$style("#grey_chelle{color: grey;
                                 font-style: bold;
                                 }"
                                 )
                                 )
                               )
                               ),
                        tabItem(tabName = "tom",
                                uiOutput("which_player_tom"),
                                
                                fluidRow(
                                  column(width = 2, plotOutput("plot_a_tom", height = 95, click = "click_a_tom")),
                                  column(width = 2, plotOutput("plot_b_tom", height = 95, click = "click_b_tom")),
                                  column(width = 2, plotOutput("plot_c_tom", height = 95, click = "click_c_tom")),
                                  column(width = 2, plotOutput("plot_d_tom", height = 95, click = "click_d_tom")),
                                  column(width = 2, plotOutput("plot_e_tom", height = 95, click = "click_e_tom"))
                                ),
                                fluidRow(
                                  column(width = 2, plotOutput("plot_f_tom", height = 95, click = "click_f_tom")),
                                  column(width = 2, plotOutput("plot_g_tom", height = 95, click = "click_g_tom")),
                                  column(width = 2, plotOutput("plot_h_tom", height = 95, click = "click_h_tom")),
                                  column(width = 2, plotOutput("plot_i_tom", height = 95, click = "click_i_tom")),
                                  column(width = 2, plotOutput("plot_j_tom", height = 95, click = "click_j_tom"))
                                ),
                                fluidRow(
                                  column(width = 2, plotOutput("plot_k_tom", height = 95, click = "click_k_tom")),
                                  column(width = 2, plotOutput("plot_l_tom", height = 95, click = "click_l_tom")),
                                  column(width = 2, plotOutput("plot_m_tom", height = 95, click = "click_m_tom")),
                                  column(width = 2, plotOutput("plot_n_tom", height = 95, click = "click_n_tom")),
                                  column(width = 2, plotOutput("plot_o_tom", height = 95, click = "click_o_tom"))
                                ),
                                fluidRow(
                                  column(width = 2, plotOutput("plot_p_tom", height = 95, click = "click_p_tom")),
                                  column(width = 2, plotOutput("plot_q_tom", height = 95, click = "click_q_tom")),
                                  column(width = 2, plotOutput("plot_r_tom", height = 95, click = "click_r_tom")),
                                  column(width = 2, plotOutput("plot_s_tom", height = 95, click = "click_s_tom")),
                                  column(width = 2, plotOutput("plot_t_tom", height = 95, click = "click_t_tom"))
                                ),
                                fluidRow(
                                  column(width = 2, plotOutput("plot_u_tom", height = 95, click = "click_u_tom")),
                                  column(width = 2, plotOutput("plot_v_tom", height = 95, click = "click_v_tom")),
                                  column(width = 2, plotOutput("plot_w_tom", height = 95, click = "click_w_tom")),
                                  column(width = 2, plotOutput("plot_x_tom", height = 95, click = "click_x_tom")),
                                  column(width = 2, plotOutput("plot_y_tom", height = 95, click = "click_y_tom"))
                                ),
                                useShinyjs(),
                                uiOutput('done_button_tom'),
                                fluidRow(
                                  column(width = 3, uiOutput('green_tom')),
                                  tags$head(tags$style("#green_tom{color: green;
                                 font-style: bold;
                                 }"
                                  )
                                  ),
                                  column(width = 3, uiOutput('black_tom')),
                                  tags$head(tags$style("#black_tom{color: black;
                                 font-style: bold;
                                 }"
                                  )
                                  ),
                                  column(width = 3, uiOutput('grey_tom')),
                                  tags$head(tags$style("#grey_tom{color: grey;
                                 font-style: bold;
                                 }"
                                  )
                                  )
                                )
                        ),
                        tabItem(tabName = "settings", 
                                helpText("Not possible at the moment, so contact Tom.")
                        )
                      )
                    )

)

