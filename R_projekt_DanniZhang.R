# Loeb andmestiku CSV-failist.
data = read.csv("C:/Users/bert/Downloads/Sleep_health_and_lifestyle_dataset.csv", header = TRUE, sep = ",")
summary(data)
nrow(data)
ncol(data)

#Eraldan vererõhu andmed süstoolseks ja diastoolseks, muutes need numbriteks. 
#Muudan ka KMI kategooria väärtuse.
library(dplyr)
library(stringr)
data <- data %>%
  mutate(
    Systoolne = as.numeric(str_split_fixed(Blood.Pressure, "/", 2)[, 1]),
    Diastoolne = as.numeric(str_split_fixed(Blood.Pressure, "/", 2)[, 2])
  )
data$BMI.Category <- recode(data$BMI.Category, 
                              "Normal Weight" = "Normal")

#Arvutan ja vormindan erinevate numbriliste muutuja kirjeldavad statistilised näitajad.
library(psych)
arvandmed <- describe(data[, c("Age", "Sleep.Duration", "Physical.Activity.Level", 
                                       "Systoolne", "Diastoolne", "Heart.Rate", "Daily.Steps")])
arvandmed <- arvandmed %>%
  mutate(across(where(is.numeric), ~ round(., 1)))

arvandmed <- arvandmed %>%
  select(-trimmed, -mad, -skew, -kurtosis, -vars, -range, -se, -n)

arvandmed <- arvandmed %>%
  mutate(
    Keskmine_SD = paste0(mean, " ± ", sd),
    Mediaan_Vahemik = paste0(median, " [", min, " - ", max, "]")
  ) %>%
  select(-mean, -sd, -median, -min, -max)

rownames(arvandmed) <- c("Vanus", "Une_kestus",
                                 "Aktiivsustase" , "Systoolne", 
                                 "Diastoolne", "Pulss", 
                                 "Sammud")

#Arvutan sagedaseima väärtuse ja selle osakaalu kategoorilistele muutujatele.
#Funktsioon 'get_mode' leiab vektorist kõige sagedasema väärtuse (moode).
get_mode <- function(v) {
  uniqv <- unique(v)
  uniqv[which.max(tabulate(match(v, uniqv)))]
}
# Loob andmetabeli, kus iga kategoorilise muutuja jaoks arvutatakse mood.
kategorilised_andmed <- data.frame(Mood = c(get_mode(data$Gender),
                                            get_mode(data$Quality.of.Sleep), 
                                            get_mode(data$Stress.Level), 
                                            get_mode(data$BMI.Category),
                                            get_mode(data$Occupation),
                                            get_mode(data$Sleep.Disorder)))
# Funktsioon 'mode_osakaal' arvutab, kui suure protsendi mood moodustab vektori kõikidest väärtustest.
mode_osakaal <- function(x) {
  mode_value <- get_mode(x)
  round(sum(x == mode_value) / nrow(data) * 100, 2)
}
kategorilised_andmed$Osakaal <- c(mode_osakaal(data$Gender),
                                  mode_osakaal(data$Quality.of.Sleep), 
                                  mode_osakaal(data$Stress.Level), 
                                  mode_osakaal(data$BMI.Category),
                                  mode_osakaal(data$Occupation),
                                  mode_osakaal(data$Sleep.Disorder))
# Määrab ridade nimed, et kajastada iga kategoorilise muutuja kirjeldust.
rownames(kategorilised_andmed) <- c("Sugu", "Une_kvaliteet", "Stressitase", "KMI", "Elukutse", "Unehaired")
# Vormindab osakaalu veeru, lisades protsendimärgi (%).
kategorilised_andmed <- kategorilised_andmed %>%
  mutate(Osakaal = paste0(Osakaal, "%"))

#Loon graafiku, mis näitab stressitaseme ja unekvaliteedi seost, eristades sugusid.
library(ggplot2)

ggplot(data, aes(x = Stress.Level, y = Quality.of.Sleep, color = Gender)) +
  geom_point() + 
  geom_smooth(method = "lm") + 
  labs(x = "Stressitase (1-10)",
       y = "Unekvaliteet (1-10)",
       color = "Sugu") +
  scale_color_manual(values = c("Male" = "blue", "Female" = "red"),
                     labels = c("Naine", "Mees"))

#Arvutan ja kuvan korrelatsioonikordajad unekvaliteedi, stressitaseme ja soo vahel.
data$Gender_numeric <- as.numeric(as.factor(data$Gender))
correlation <- cor(data$Quality.of.Sleep, data$Gender_numeric, method = "spearman")
print(correlation)
correlation2 <- cor(data$Stress.Level, data$Gender_numeric, method = "spearman")
print(correlation2)

#Muudan kategoorilised muutujad numbrilisteks väärtusteks, et arvutada korrelatsioonimaatriks. 
library(reshape2)
unique(data$Sleep.Disorder)
data$KMI_numeric <- as.numeric(factor(data$BMI.Category, 
                                      levels = c("Normal", "Overweight", "Obese"),
                                      labels = c(1, 2, 3)))
data$unehaired_numeric <- as.numeric(factor(data$Sleep.Disorder, 
                                      levels = c("None", "Sleep Apnea", "Insomnia"),
                                      labels = c(1, 2, 3)))

#Valin numbrilised muutujad ja arvutan Spearmani korrelatsioonimaatriksi. 
#Visualiseerib maatriksi soojuskaardina, kus erinevad värvid näitavad seose tugevust.
numbriline_andmed <- data %>%
  select(Vanus = Age, Une_kestus = Sleep.Duration, Aktiivsustase = Physical.Activity.Level,
         Pulss = Heart.Rate, Sammud = Daily.Steps, Systoolne = Systoolne, Diastoolne = Diastoolne,
         Une_kvaliteet = Quality.of.Sleep, Stressitase = Stress.Level, KMI = KMI_numeric, unehaired = unehaired_numeric)
# Arvutab Spearmani korrelatsioonikordajad numbriliste muutujate vahel.
correlation_matrix <- cor(numbriline_andmed, method = "spearman", use = "complete.obs")
# Sulatab korrelatsioonimaatriksi pikkade andmete formaati, mis sobib ggplot-i kasutamiseks.
melted_cor <- melt(correlation_matrix)
ggplot(data = melted_cor, aes(x = Var1, y = Var2, fill = value)) +
  geom_tile(color = "white") +
  scale_fill_gradient2(low = "blue", high = "red", mid = "white", 
                       midpoint = 0, limit = c(-1,1), space = "Lab", 
                       name="Korrelatsioon") +
  theme_minimal() + 
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1)) +
  geom_text(aes(label = round(value, 2)), color = "black", size = 4) # Lisab ruutudele tekstiga korrelatsiooniväärtused, ümardades need kahe komakohani.

#Teostab Kruskali-Wallise testi, et kontrollida, kas amet mõjutab unekvaliteeti. 
kruskal_result <- kruskal.test(Quality.of.Sleep ~ Occupation, data = data)
print(kruskal_result)

#Grupeerib une kestuse ja kvaliteedi kategooriatesse ning kuvab nende jaotuse virnpäisena. 
data$Sleep_Duration_Group <- cut(
  data$Sleep.Duration,
  breaks = c(5.7, 6.7, 7.6, 8.6),           
  labels = c("Lühike", "Keskmine", "Pikk")  
)

data$Sleep_quality_Group <- cut(
  data$Quality.of.Sleep,
  breaks = c(3.9, 6, 7.5, 9.1),           
  labels = c("halb", "keskmine", "hea")
)

ggplot(data, aes(x = Sleep_Duration_Group, fill = Sleep_quality_Group)) +
  geom_bar(position = "stack") +  
  labs(
    x = "Une kestuse grupp",
    y = "Kogus",
    fill = "Une kvaliteedi grupp"
  ) +
  scale_fill_manual(values = c("keskmine" = "orange", "hea" = "darkgreen", "halb" = "darkred"))

#Visualiseerib stressitaseme jaotuse une kvaliteedi gruppides.
data$Stress_Level_Group <- cut(
  data$Stress.Level,
  breaks = c(2.9, 4.5, 6.5, 8.1),           
  labels = c("madal", "keskmine", "kõrge")
)
ggplot(data, aes(x = Sleep_quality_Group, fill = Stress_Level_Group)) +
  geom_bar(position = "stack") +  
  labs(
    x = "Une kvaliteedi grupp",
    y = "Kogus",
    fill = "Stressitase grupp"
  ) +
  scale_fill_manual(values = c("kõrge" = "darkred", "keskmine" = "orange", "madal" = "darkgreen"))

#Visualiseerib KMI jaotuse une kvaliteedi gruppides.
data$BMI.Category <- factor(data$BMI.Category, 
                            levels = c("Normal", "Overweight", "Obese"), 
                            labels = c("Normaalne", "Ülekaal", "Rasvumine"))
ggplot(data, aes(x = Sleep_quality_Group, fill = BMI.Category)) +
  geom_bar(position = "stack") +  
  labs(
    x = "Une kvaliteedi grupp",
    y = "Kogus",
    fill = "KMI kategooria"
  )  +
  scale_fill_manual(values = c("Rasvumine" = "darkred", "Ülekaal" = "orange", "Normaalne" = "darkgreen"))

#Visualiseerib une kvaliteedi jaotuse ametigruppide kaupa ja 
#kuvab keskmise stressitaseme samas graafikus teisese telje abil.
data_summary <- data %>%
  group_by(Occupation) %>%
  summarise(Mean_Stress = mean(Stress.Level, na.rm = TRUE))
## Skaalategur teisese telje jaoks, et joondada stressitaseme väärtused.
scale_factor <- 4
ggplot(data, aes(x = Occupation)) +
  geom_bar(aes(fill = Sleep_quality_Group), position = "stack") + 
  geom_line(data = data_summary, aes(x = Occupation, y = Mean_Stress * scale_factor, group = 1), 
            linewidth = 1) + # Lisab joone keskmise stressitaseme jaoks, korrutades väärtuse skaalateguriga.
  geom_point(data = data_summary, aes(x = Occupation, y = Mean_Stress * scale_factor), 
             size = 2) + # Lisab punktid, mis tähistavad keskmist stressitaset igas ametigrupis.
  scale_y_continuous(
    name = "Kogus",
    sec.axis = sec_axis(~ . / scale_factor, name = "Keskmine stressitase")
  ) +
  labs(
    x = "Amet",
    fill = "Une kvaliteedi grupp",
  ) +
  scale_fill_manual(values = c("keskmine" = "orange", "hea" = "darkgreen", "halb" = "darkred")) +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1)
  ) # Pöörab x-telje teksti 45 kraadi, et hõlbustada loetavust.

