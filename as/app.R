
# Definindo o UI
ui <- dashboardPage(
  dashboardHeader(title = "Análise de Dados do DataSenado"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Preocupações", tabName = "preocupacoes"),
      menuItem("Opiniões", tabName = "opinioes"),
      menuItem("Outras Análises", tabName = "outras_analises")
    )
  ),
  dashboardBody(
    tabItems(
      tabItem(
        tabName = "preocupacoes",
        h2("Principais Preocupações"),
        fluidRow(
          box(plotOutput("plot_preocupacoes"), width = 12)
        )
      ),
      tabItem(
        tabName = "opinioes",
        h2("Opiniões sobre Questões"),
        fluidRow(
          box(plotOutput("plot_opinioes_1"), width = 6),
          box(plotOutput("plot_opinioes_2"), width = 6),
          box(plotOutput("plot_opinioes_3"), width = 12)
        )
      ),
      tabItem(
        tabName = "outras_analises",
        h2("Outras Análises"),
        fluidRow(
          box(plotOutput("plot_maioridade_sexo"), width = 6),
          box(plotOutput("plot_maioridade_voto1"), width = 6),
          box(plotOutput("plot_maioridade_voto2"), width = 12)
        ),
        fluidRow(
          box(plotOutput("plot_impostos_religiao"), width = 12)
        ),
        fluidRow(
          box(plotOutput("plot_valor_fortuna"), width = 12)
        )
      )
    )
  )
)

server <- function(input, output) {
  output$plot_preocupacoes <- renderPlot({
    ggplot(preocupacao, aes(x = Preocupação, y = (porcentagem*100), fill = Preocupação)) +
      geom_bar(stat = "identity") +
      labs(title = "Principais preocupações dos respondentes", y = "Porcentagem (%)", x = "Categoria") +
      theme_minimal() +
      theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
      scale_fill_brewer(palette = "Set3")
  })
  
  output$plot_opinioes_1 <- renderPlot({
    ggplot(maioridADE, aes(x = Opinião, y = (porcentagem*100), fill = Opinião)) +
      geom_bar(stat = "identity") +
      labs(title = "Opinião sobre a redução da maioridade penal", y = "Porcentagem (%)", x = "Opinião") +
      theme_minimal() +
      theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
      scale_fill_brewer(palette = "Set3")
  })
  
  output$plot_opinioes_2 <- renderPlot({
    ggplot(impo_fortuna, aes(x = Opinião, y = (porcentagem*100), fill = Opinião)) +
      geom_bar(stat = "identity") +
      labs(title = "Opinião sobre criar impostos sobre grandes fortunas", y = "Porcentagem (%)", x = "Opinião") +
      theme_minimal() +
      theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
      scale_fill_brewer(palette = "Set3")
  })
  
  output$plot_opinioes_3 <- renderPlot({
    ggplot(opi_por_rel, aes(x = Religião, y = (porcentagem*100), fill = Opinião)) +
      geom_bar(stat = "identity", position = "dodge") +
      labs(title = "Opinião sobre a redução da maioridade penal por religião", y = "Porcentagem (%)", x = "Religião") +
      theme_minimal() +
      theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
      scale_fill_brewer(palette = "Set3")
  })
  
  output$plot_maioridade_sexo <- renderPlot({
    ggplot(preocupacao_por_sexo, aes(x = Preocupação, y = (porcentagem*100), fill = Sexo)) +
      geom_bar(stat = "identity", position = "dodge") +
      labs(title = "Principais preocupações dos respondentes por sexo", y = "Porcentagem (%)", x = "Categoria") +
      theme_minimal() +
      theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
      scale_fill_brewer(palette = "Set3")
  })
output$plot_maioridade_voto1 <- renderPlot({
    ggplot(opi_1_tur, aes(x = Opinião, y = (porcentagem*100), fill = `Voto no 1° Turno`)) +
      geom_bar(stat = "identity", position = "dodge") +
      labs(title = "Opinião sobre a redução da maioridade penal por quem votou na última eleição (Voto no 1° Turno)", y = "Porcentagem (%)", x = "Opinião") +
      theme_minimal() +
      theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
      scale_fill_brewer(palette = "Set3")
  })
  
  output$plot_maioridade_voto2 <- renderPlot({
    ggplot(opi_2_tur, aes(x = Opinião, y = (porcentagem*100), fill = `Voto no 2° Turno`)) +
      geom_bar(stat = "identity", position = "dodge") +
      labs(title = "Opinião sobre a redução da maioridade penal por quem votou na última eleição (Voto no 2° Turno)", y = "Porcentagem (%)", x = "Opinião") +
      theme_minimal() +
      theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
      scale_fill_brewer(palette = "Set3")
  })
  
  output$plot_impostos_religiao <- renderPlot({
    ggplot(opi_por_rel, aes(x = Religião, y = (porcentagem*100), fill = Opinião)) +
      geom_bar(stat = "identity", position = "dodge") +
      labs(title = "Opinião sobre criar impostos sobre grandes fortunas por Religião", y = "Porcentagem (%)", x = "Religião") +
      theme_minimal() +
      theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
      scale_fill_brewer(palette = "Set3")
  })
  
  output$plot_valor_fortuna <- renderPlot({
    ggplot(fortuna, aes(x = `Faixa de Valores`, y = (porcentagem*100), fill = `Faixa de Valores`)) +
      geom_bar(stat = "identity") +
      labs(title = "A partir de que valor seria uma grande fortuna?", y = "Porcentagem (%)", x = "Faixa de Valores") +
      theme_minimal() +
      theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
      scale_fill_brewer(palette = "Set3")
  })
}

# Rodando o aplicativo
shinyApp(ui, server)
