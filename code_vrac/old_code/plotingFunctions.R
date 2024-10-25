#*******************************************************************************
#* Created 16/11/2022
#* Creator N.P
#* Plotingfunctions
#*******************************************************************************
plot_basic <- function(TIBBLE, xaxis, yaxis, tittle_name, y_name, x_name){
  graph <- ggplot(TIBBLE) +
    geom_line(aes(x = xaxis, y = yaxis)) +
    theme(plot.title = element_text(face = "bold", size = 14, hjust = 0.5),
          axis.title = element_text(face = "bold", size = 12)) +
    scale_x_date(date_labels = "%m/%Y") +
    scale_y_continuous(labels = function(x) format(x, scientific = TRUE, digits = 10)) +
    labs(title = tittle_name,
         x = x_name,
         y = y_name)
  graph
}

plot_barrage<- function(interannual_mean_barrage){
  graph_Barrage <- ggplot(interannual_mean_barrage) +
    geom_line(aes(x = date, y = MeanS)) +
    # geom_line(aes(x = date, y = Volume_FO)) +
    theme(plot.title = element_text(face = "bold", size = 14, hjust = 0.5),
          axis.title = element_text(face = "bold", size = 12)) +
    facet_wrap(~Nom, scales = 'free_y') +
    scale_x_date(date_labels = "%b") +
    scale_y_continuous(labels = function(x) format(x, scientific = TRUE, digits = 10)) +
    labs(title = "Evolution interannuelle des stockages d'eau dans les différentes retenues",
         x = "Mois de l'année",
         y = "Volume moyen interannuel (Mm³)")
  graph_Barrage
}

plot_FO_barrage <- function(interannual_mean_barrage){
  graph_FO_barrage <- ggplot(interannual_mean_barrage) +
    geom_line(aes(x = date, y = Volume_FO)) +
    # geom_line(aes(x = date, y = dMeanS), color = 'grey') +
    theme(plot.title = element_text(face = "bold", size = 14, hjust = 0.5),
          axis.title = element_text(face = "bold", size = 12)) +
    facet_wrap(~Nom, scales = 'free_y') +
    scale_x_date(date_labels = "%b") +
    labs(title = "Fonctions Objectifs: gestion des stockages d'eau dans les différentes retenues",
         x = "Mois de l'année",
         y = "Volume objectif (m³)")
  return(graph_FO_barrage)
}



plot_transfert <- function(interannual_mean_transfert){
  
  
  graph_Transfert <- ggplot(interannual_mean_transfert) +
    geom_line(aes(x = date, y = MeanQ)) +
    geom_line(aes(x = date, y = Volume_FO), color = 'grey') +
    theme(plot.title = element_text(face = "bold", size = 14, hjust = 0.5),
          axis.title = element_text(face = "bold", size = 12)) +
    facet_wrap(~Nom, scales = 'free_y') +
    scale_x_date(date_labels = "%b") +
    scale_y_continuous(labels = function(x) format(x, scientific = TRUE)) +
    labs(title = "Evolution interannuelle des transferts d'eau",
         x = "Mois de l'année",
         y = "Débit moyen interannuel (m³/s)")
  return (graph_Transfert)
}

plot_affluent<- function(interannual_mean_affluent){
  graph_Affluent <- ggplot(interannual_mean_affluent) +
    geom_line(aes(x = date, y = MeanQ)) +
    theme(plot.title = element_text(face = "bold", size = 14, hjust = 0.5),
          strip.text.x = element_text(size=10, face="bold"),
          axis.title = element_text(face = "bold", size = 12)) +
    facet_wrap(vars(ordre_Amont_Aval,Nom_lieu, code_station), scales = 'free_y', ncol = 6) +
    scale_x_date(date_labels = "%b") +
    labs(title = 'Evolution interannuelle des débits dans les différents affluents',
         x = "Mois de l'année",
         y = "Débit moyen interannuel (m³/s)")
  return(graph_Affluent)
}

plot_derivation <- function(data, timelimit, name){
  graph <- ggplot(data[data$Date<= timelimit,]) +
    geom_line(aes(x = Date, y = volume_removed)) +
    geom_line(aes(x = Date, y = Volume_FO), color = "red") +
    theme(plot.title = element_text(face = "bold", size = 14, hjust = 0.5),
          axis.title = element_text(face = "bold", size = 12)) +
    scale_x_date(date_labels = "%b-%y") +
    scale_y_continuous(labels = function(x) format(x, scientific = TRUE, digits = 10)) +
    labs(title = name,
         x = "Mois de l'année",
         y = "Volume (L)")
  graph
}

plot_taux_derivation <- function(data, meanvalue, name){
  graph <- ggplot(data) +
    geom_line(aes(x = Date, y = taux_prelevement)) +
    geom_hline(yintercept = meanvalue, color = "red") +
    theme(plot.title = element_text(face = "bold", size = 14, hjust = 0.5),
          axis.title = element_text(face = "bold", size = 12)) +
    scale_x_date(date_labels = "%m-%Y") +
    labs(title = name,
         x = "Temps",
         y = "Taux de prelevement (%)")
  graph
}