library(stringr)
library(dplyr)
install.packages("ggplot2")
install.packages("reshape2")
library(ggplot2)
library(reshape2)
install.packages("leaps")
install.packages("glmnet")
install.packages("rpart")
library(glmnet)
install.packages('Metrics')
library(Metrics)
install.packages('pls')
library(pls)
install.packages("stargazer")
library(stargazer)
install.packages("ggthemes")
library(ggthemes)

set.seed(173)
dati1<-read.csv("stat giocatori ufficiale.csv", sep = ";")
dati2<-read.csv("salari giocatori ufficiale.csv", sep=";")
#per ogni osservazione, se nella colonna 7 abbiamo unverified estimation, cioè l'annual wage è una stima,
#e non è confermato, lo prendo in considerazione come un NA.
for (i in 1:nrow(dati2)) {
  if (dati2$Notes[i] == "Unverified estimation") {
    dati2$Annual.Wages[i] <- NA 
  }
}
#quanti na ci sono nella variabile annual wages e qual'è la percentuale in proporzione al numero totale
#di osservazioni
na_annualwages<-sum(is.na(dati2$Annual.Wages))
na_annualwages/nrow(dati2)*100

# Funzione per trasformare i valori di Annual.Wages in euro
trasforma_valore_euro <- function(valore) {
  euro_value <- str_extract(valore, "(?<=€ )[0-9.,]+")
  euro_value <- str_replace_all(euro_value, ",", "")
  as.numeric(euro_value)
}
# Applica la trasformazione a ogni osservazione della colonna "Annual.Wages" di "dati2"
dati2 <- dati2 %>% mutate(Annual.Wages = trasforma_valore_euro(Annual.Wages))
class(dati2$Annual.Wages)
sum(dati2$Annual.Wages, na.rm = TRUE)


#unisco i 2 dataset raggruppandoli per nome cognome ed età, e per ogni corrispondenza aggiungo la colonna 
#annual wages nel datset dati 1, per i giocatori la quale non c'e corrispondenza,
#metto na come valore di annual wages.
dati2 <- dati2 %>% mutate(Age = as.integer(Age))
merged_dataset <- left_join(dati1, dati2, by = c("Player", "Age"))

#notiamo che le osservazioni sono aumentate, questo ci suggerisce che che ci sono delle osservazioni multiple nel dataset dati2 che corrispondono alle righe in dati1 basate 
#sulle colonne chiave specificate (in questo caso "nome_del_giocatore" e "età").
#In altre parole, per alcune combinazioni di "nome_del_giocatore" e "età" nel dati1, 
#esistono più di una corrispondenza nel dati2, e dunque left_join() crea righe aggiuntive per 
#ciascuna di queste corrispondenze.
#Ecco come puoi indagare ulteriormente: Verifica le osservazioni duplicate in dati2:
#Puoi verificare se ci sono combinazioni duplicate di "nome_del_giocatore" e "età" nel dati2:
#Se duplicates contiene righe, significa che ci sono combinazioni di "nome_del_giocatore" e "età" nel dati2 che appaiono più di una volta.
#Decisione sul come procedere:A seconda della natura dei tuoi dati e di cosa intendi fare, potresti decidere di:
#Mantenere la prima corrispondenza: Usando distinct() per rimuovere le osservazioni duplicate in dati2 prima dell'unione.
#Aggregare i dati in dati2: Se ha senso aggregare i dati (ad es. prendendo la media se i dati sono numerici) per ogni combinazione di "nome_del_giocatore" e "età", 
#potresti farlo prima di eseguire l'unione.

#verifico duplicati in dati2
library(dplyr)
duplicates <- dati2 %>%
  group_by(Player, Age,Nation) %>%
  filter(n() > 1)
#122 osservazioni sono duplicate nel dataset dati2, #questo è dovuto al fatto che alcuni giocatori possono essere andati in prestito od esser 
#stati ceduti, a gennaio, o dopo l'inizio del campionato, e quindi presentano uno stipendio per ogni squadra in cui
#hanno militato nella stagione 2023/2024
#decido di riaggruppare le osservazioni duplicate, facendo la media dei loro stipendi dove possibile.


dati2 <- dati2 %>%
  group_by(Player, Nation,Age) %>%
  summarize(Annual.Wages = mean(Annual.Wages))





merged_dataset <- left_join(dati1, dati2, by = c("Player", "Age"))
#rimuovo le variabili che si ripetevano nei 2 datset
merged_dataset <- select(merged_dataset, -c(Nation.y))
sum(dati2$Annual.Wages, na.rm = TRUE)
sum_by_campionato <- aggregate(Annual.Wages ~ Comp, data = merged_dataset, FUN = sum)
print(sum_by_campionato)


na_annualwages2<-sum(is.na(merged_dataset$Annual.Wages))
na_annualwages2/nrow(merged_dataset)*100

#rinomino la variabile nation.x in nation
names(merged_dataset)[names(merged_dataset) == "Nation.x"] <- "Nation"

#creo delle variabili binarie per la posizione dei  giocatori, in quanto un giocatore nella variabile pos
#puo assumere anche 2 modalità, ovvero per esempio ha giocato nell'arco della stagione, sia da centrocampista,
#che da attaccante
posizioni_uniche <- unique(str_split(merged_dataset$Pos, ",", simplify = TRUE))
for (posizione in posizioni_uniche) {
  merged_dataset <- merged_dataset %>% mutate(!!paste0("Pos_", posizione) := as.numeric(str_detect(Pos, posizione)))
}
#rimuovo la colonna pos iniziale, di qui ora non me ne faccio più nulla.



#vado a vedere quanti duplicati ci sono nel dataset
duplicates1 <- merged_dataset %>%
  group_by(Player, Age,Nation) %>%
  filter(n() > 1)

nrow(duplicates1)/nrow(merged_dataset)
# i giocatori duplicati, cioè quei giocatori che hanno cambiato squadra nel corso della stagione sono 322
#circa il 10% del dataset totale. Questi giocatori sono di difficile interpretazione in quanto presentano
#una doppia osservazione per ogni variabile nel datset. Si potrebbe cercare di sommare le variabili per aggregare
#le osservazioni o fare una media come effettuato prima. ma entrambi i metodi non avrebbero senso, in quanto
#alcune variabili se fatta la media, andrebbero a dare una falsa indicazione, come la media dei gol al posto che sommarli,
#o anche i minuti giocati che andrebbero sommati. dall'altro canto svolgendo la somma altre variabili non avrebbero senso cioè quelle predittive,
#come gol per minuti giocati, gol per 90 minuti, assit per 90 minuti e molte altre.
#per semplicità, decido quindi di rimuoverle

dataset <- anti_join(merged_dataset, duplicates1, by = c("Player", "Age", "Nation"))

dataset$Min <- as.numeric(gsub(",", "", dataset$Min))
#comp da charachter a factor
dataset <- dataset %>% mutate(Comp = as.factor(Comp))
levels(dataset$Comp)
#squad da charachter a factor
dataset <- dataset %>% mutate(Squad = as.factor(Squad))
levels(dataset$Squad)
#nation da charachter a factor
dataset <- dataset %>% mutate(Nation = as.factor(Nation))
levels(dataset$Nation)


#verificare la corretta classe delle variabili.
variable_classes <- sapply(dataset, class)
print(variable_classes)


#VALORI MANCANTI
#valori mancanti di annual wages
sum(is.na(dataset$Annual.Wages))
sum(is.na(dataset$Annual.Wages))/nrow(dataset)*100

# Calcola il numero di NA per ogni variabile
conteggio_na <- sapply(dataset, function(col) any(is.na(col)))

# Conta quante variabili hanno almeno un NA
numero_variabili_con_na <- sum(conteggio_na)

print(numero_variabili_con_na)
numero_variabili_con_na/ncol(dataset)*100


#guardo osservazioni che hanno più del 50% di na
# Trova le righe con più del 50% di NA
NA50<-apply(dataset, 1, function(row) sum(is.na(row)) > (ncol(dataset) * 0.6))
df_na <- dataset[NA50, ]
#rimuovo le righe
dataset <- dataset %>%
  anti_join(df_na, by = c("Player", "Nation", "Age")) # sostituisci "colonna1", "colonna2", ecc. con le colonne chiave che hai utilizzato per identificare le osservazioni nel gruppo df_na


#numero di NA per ogni variabile
na_count <- sapply(dataset, function(x) sum(is.na(x)))
print(na_count)

dataset[is.na(dataset$Succ), ]
dataset<-dataset[-c(1670,2129),]


#variabili che presentano almeno un NA
conteggio_na <- sapply(dataset, function(col) sum(is.na(col)))
# Filtriamo solo le variabili che hanno almeno un NA, lò
conteggio_na_con_valori <- conteggio_na[conteggio_na > 0]
print(conteggio_na_con_valori)

#osservazioni che presentano almeno un na
numero_osservazioni_con_na <- sum(apply(dataset, 1, function(row) any(is.na(row))))
print(numero_osservazioni_con_na)
12/ncol(dataset)*100
1831/nrow(dataset)*100

# Identifica le colonne con NA escludendo Annual.wages
cols_to_remove <- names(dataset)[apply(dataset[, !(names(dataset) == "Annual.Wages")], 2, anyNA)]

# Rimuovi le colonne identificate
dataset <- dataset[, !names(dataset) %in% cols_to_remove]

#CORRELAZIONI QUALITATIVE DA RIMUOVERE.
correlazione<-dataset
correlazione <- correlazione[, -which(names(correlazione) %in% c('Player','Nation', 'Squad'))]
cor_matrix <- cor(correlazione, use = "pairwise.complete.obs")  # calcola su osservazioni complete

# Inizializzare un dataframe vuoto per memorizzare i risultati
high_cor_vars <- data.frame(
  Var1 = character(),
  Var2 = character(),
  Correlation = numeric()
)

# Trovare coppie di variabili con alta correlazione
for(i in 1:(ncol(cor_matrix) - 1)) {
  for(j in (i + 1):ncol(cor_matrix)) {
    cor_value <- cor_matrix[i, j]
    if(abs(cor_value) > 0.95) {
      high_cor_vars <- rbind(
        high_cor_vars, 
        data.frame(
          Var1 = colnames(cor_matrix)[i],
          Var2 = colnames(cor_matrix)[j],
          Correlation = cor_value
        )
      )
    }
  }
}

# Visualizzare le variabili altamente correlate
high_cor_vars


dataset <- dataset[, -which(names(dataset) %in% c('Pos_','Pos',"PK", "Def.1","PK.1", "PKatt.1", "PrgC.1", "PrgP.1",'PrgR.1','Born','X90s', 'G.PK','npxG','G.PK.1','G.A.PK','npxG.1','npxG.xAG.1','np.G.xG','Att','Att.2','Live','PrgDist.1','Tkl.1','Tkl.Int','G.A','G.xG','A.xAG','Def','Att.3rd.1','Pass'))]
dataset$Comp<-as.numeric(dataset$Comp)
dataset <- dataset[dataset$Pos_GK == 0, ]
dataset$Pos_GK<-NULL
# Caricare il pacchetto dplyr
library(dplyr)

squad_wages_sum <- dataset %>%
  group_by(Squad) %>%
  summarise(Total_Annual_Wages = sum(Annual.Wages, na.rm = TRUE)) %>%
  arrange(desc(Total_Annual_Wages))  # Ordina in base a Total_Annual_Wages in ordine decrescente

# Visualizza il risultato
print(squad_wages_sum)
squad_wages_sum$Group <- cut(squad_wages_sum$Total_Annual_Wages, breaks = 10, labels = 1:10)

# Unisci il nuovo dataframe con il dataframe originale
dataset <- dataset %>%
  left_join(squad_wages_sum[, c("Squad", "Group")], by = "Squad")

# Rinomina la colonna Group come 'Squad_Group'
dataset <- dataset %>%
  rename(Squad_Group = Group)

# Visualizza il risultato
head(dataset)
dataset$Squad<-NULL

#DIVISIONE IN TRAINING, VALIDATION E TEST




trn_idx <- sample(nrow(dataset), 0.67*nrow(dataset))
training_set<- dataset[trn_idx,]
test_set <- dataset[-trn_idx,]



# Esempio di creazione di una variabile fattoriale
training_set$Player <- as.factor(training_set$Player)
training_set$Player <- as.numeric(training_set$Player)



training_set <- training_set[, -which(names(training_set) %in% c( 'Nation', 'Player'))]

# Supponiamo che `data_frame` sia il tuo data frame e `variabile` sia la colonna che vuoi controllare



sum(is.na(dataset$Annual.Wages))

training_set<-na.omit(training_set)

# Carica il pacchetto "leaps"
library(leaps)

training_set$Annual.Wages<-log(training_set$Annual.Wages)
# Supponiamo che tu abbia già diviso il tuo dataset in training_set (con le variabili indipendenti e dipendenti)
# Addestramento di un modello di regressione lineare multipla sul training set
model_lm <- lm(Annual.Wages ~ ., data = training_set_standardized)
summary(model_lm)
exp(model_lm$fitted.values)



training_set_standardized <- as.data.frame(lapply(training_set[,-c(87,83)], scale))

training_set_standardized$Squad_Group<-training_set$Squad_Group
training_set_standardized$Annual.Wages<-training_set$Annual.Wages
dataset$Nation<-NULL
dataset_standardized <- as.data.frame(lapply(dataset[,-c(88,84, 2)], scale))
dataset_standardized$Squad_Group<-dataset$Squad_Group
dataset_standardized$Annual.Wages<-dataset$Annual.Wages
dataset_standardized$Player<-dataset$Player
dataset_standardized<-na.omit(dataset_standardized)
dataset<-na.omit(dataset)
dataset_standardized$Annual.Wages<-log(dataset_standardized$Annual.Wages)
prediction_step_test<-exp(predict(stepwise_model, newdata = dataset_standardized))
prediction_step_test
dataset$salary_predicted <- prediction_step_test
dataset$salary_difference <- dataset_standardized$salary_predicted - exp(dataset_standardized$Annual.Wages)

# Seleziona le colonne di interesse
result <- dataset %>% 
  select(Player, Squad_Group, salary_difference) %>% 
  arrange((salary_difference)) # ordina in base alla differenza per visualizzare i calciatori più sopravvalutati o sottovalutati

# Visualizza il risultato
head(result)



#STEPWISE MODELLO NON VA BENE CON P-VALUE

stepwise_model <- step(model_lm, direction = 'backward')
stepwise_model$call
#lm(formula = Annual.Wages ~ Rk + Age + Ast + CrdY + xG.1 + Sh.90 + 
#FK + PrgDist + Att.1 + Att.3 + CrsPA + SCA + SCA90 + PassLive + 
#PassDead + TO + Sh.1 + GCA + GCA90 + Def.3rd + Mid.3rd + 
#Att.3rd + Carries + CPA + Mis + Lost + Blocks + Int + Clr + 
#Squad_Group, data = training_set_standardized)
predictions_step <- exp(predict(stepwise_model, newdata = training_set_standardized))
predictions_step
# Calcola l'MSE per il modello PCR
mse_step <- mean((predictions_step - exp(training_set_standardized$Annual.Wages))^2)
mse_step#7.795914e+12
test_set$Nation<-NULL
train<-training_set[, -c(83,87)]
means_training <- colMeans(train)
sds_training <- apply(train, 2, sd)
test_set_standardized <- as.data.frame(scale(test_set[,-c(2,84,88)], center = means_training, scale = sds_training))
test_set_standardized$Annual.Wages<-test_set$Annual.Wages
test_set_standardized$Squad_group<-NULL
test_set_standardized$Squad_Group<-test_set$Squad_Group



test_set_standardized<-na.omit(test_set_standardized)
test_set_standardized$Annual.Wages<-log(test_set_standardized$Annual.Wages)
prediction_step_test<-exp(predict(stepwise_model, newdata = test_set_standardized))
prediction_step_test
mse_step_test <- mean((prediction_step_test - exp(test_set_standardized$Annual.Wages))^2)
mse_step_test# 1.251512e+13


# Visualizza i risultati
summary(stepwise_model)
exp(stepwise_model$fitted.values)
stepwise_model$coefficients
plot(stepwise_model)
AIC(stepwise_model)
AIC(model_lm)
head(training_set)
dev.off()
par(mfrow=c(2,2))
plot(stepwise_model)
res.stud<-rstandard(stepwise_model) 
par(mfrow=c(1,1))
qqnorm(res.stud)
qqline(res.stud) 
residui.raw<-residuals(stepwise_model)
shapiro.test(residui.raw)

par(mfrow=c(1,1))
library(carData)
library(car)
influencePlot(stepwise_model) 
outlierTest(stepwise_model)
influenceIndexPlot(stepwise_model)
dataset[1200,]
dataset[673,]
dataset[1379,]
#----------------------------------------------------------------------------------------
#-------------------------------------------------------------------------------------


#-----------------------------------------------------------------------------------
#------------------------------------------------------------------------------------







head(training_set)
dev.off()
par(mfrow=c(2,2))
plot(model_lm)
res.stud<-rstandard(model_lm) 
par(mfrow=c(1,1))
qqnorm(res.stud)
qqline(res.stud) 
residui.raw<-residuals(model_lm)
shapiro.test(residui.raw)

par(mfrow=c(1,1))
library(carData)
library(car)
influencePlot(model_lm) 
outlierTest(model_lm)
influenceIndexPlot(model_lm)
training_set[training_set$Rk==199,]
dataset[2242, ]
dataset[1842, ]
dataset[578, ]
library(car)
library(ggplot2)








#ANALISI NEL TEST
test_set<-na.omit(test_set)
training_set<-na.omit(training_set)
predictions <- exp(predict(model_lm, newdata = test_set))
predictions_train <- exp(predict(model_lm, training_set))
true_values<-test_set$Annual.Wages
true_values_train<-exp(training_set$Annual.Wages)
mse_train <- mean((predictions_train - true_values_train)^2)
mse <- mean((predictions - true_values)^2)
mse_train
mse
#rmse lm training
rmse(actual = true_values_train, predicted = predictions_train)
#rmse lm test
rmse(actual = true_values, predicted = predictions)
#rmse step train
rmse(actual = true_values_train, predicted = predictions_train_step)
#rmse step test
rmse(actual = true_values, predicted = predictions_step)


predictions_step <- round(exp(predict(stepwise_model, newdata = test_set)),0)
mse_step <- mean((predictions_step - true_values)^2)
predictions_train_step <- exp(predict(stepwise_model, training_set_standardized))
mse_train_step <- mean((predictions_train_step - true_values_train)^2)
mse_step
mse_train_step

cor(predictions, true_values)^2
cor(predictions_step, true_values)^2
cor(predictions_train, true_values_train)^2
cor(predictions_train_step, true_values_train)^2

# Presupponendo che il nome del giocatore nel tuo test_set sia "PlayerName"
results_table <- data.frame(
  PlayerName = test_set$Player,
  True_Annual_Wages = test_set$Annual.Wages,
  Predicted_Annual_Wages = predictions_step
)

# Visualizza la tabella
head(results_table)

#------------------------------------------------------------------------------------------
#------------------------------------------------------------------------------------------





#CODICE PROF!!!
for (i in 1:10) {
  training_set[paste0("is_group_", i)] <- ifelse(training_set$Squad_Group == i, 1, 0)
}
X <- model.matrix(Annual.Wages ~ ., data = training_set)[, -1]
y <- training_set$Annual.Wages
n <- nrow(X)
p <- ncol(X)


training_set$Squad_Group<-NULL


# Carica il pacchetto necessario
library(dplyr)

# Crea un dataframe con solo le variabili da scalare
df_to_scale <- training_set %>% select(-Annual.Wages)

# Scala le variabili
df_scaled <- as.data.frame(scale(df_to_scale))

# Aggiungi nuovamente la variabile risposta al dataframe scalato
df_scaled$Annual.Wages <- training_set$Annual.Wages

# Ora df_scaled contiene le variabili scalate e la variabile risposta originale
head(df_scaled)

df_scaled[, 96]

pr <- princomp(df_scaled[, -96], cor = FALSE)
ggplot(data = data.frame(p = 1:p, vars = pr$sdev^2 / sum(pr$sdev^2)), aes(x = p, xmin = p, xmax = p, y = vars, ymax = vars, ymin = 0)) +
  geom_pointrange() +
  theme_light() +
  scale_color_tableau(palette = "Color Blind") +
  xlab("Numero di componenti principali") +
  ylab("Frazione di varianza spiegata")

summary(pr)

fit_pcr <- pcr(Annual.Wages ~ ., data = training_set, center = TRUE, scale = TRUE, validation = "none")

# Calcola l'MSE per ogni numero di componenti principali
mse <- sapply(1:(ncol(training_set)-1), function(num_comp) {
  pred <- exp(predict(fit_pcr, newdata = training_set, ncomp = num_comp))
  mean((pred - exp(training_set$Annual.Wages))^2)
})
# Crea un dataframe per la visualizzazione
data_mse <- data.frame(
  NumComponents = 1:(ncol(training_set)-1),
  MSE = mse
)
min_mse <- min(mse)
min_comp <- data_mse$NumComponents[which.min(data_mse$MSE)]

# Grafico
ggplot(data_mse, aes(x = NumComponents, y = MSE)) +
  geom_line() +
  geom_point() +
  geom_point(aes(x = min_comp, y = min_mse), color = "red", size = 1) +  # Punto rosso
  theme_light() +
  labs(
    x = "Numero di componenti", 
    y = "Mean Squared Error")



training_set$Annual.Wages<-exp(training_set$Annual.Wages)
fit_optimal_pcr <- pcr(Annual.Wages ~ ., data = training_set, center = TRUE, scale = TRUE,ncomp=31, validation = "none")
summary(fit_optimal_pcr)
# Calcola le previsioni sul training set
predictions_pcr <- exp((predict(fit_optimal_pcr, newdata = training_set, ncomp = 31)))
predictions_pcr
# Calcola l'MSE per il modello PCR
mse_pcr <- mean((predictions_pcr - exp(training_set$Annual.Wages))^2)
mse_pcr#6.36078e+12
coefficients <- coef(fit_optimal_pcr, ncomp = 31)
print(coefficients)
summary(fit_optimal_pcr)
# Ottenere un sommario del modello
model_summary <- summary(fit_optimal_pcr)

# Visualizzare l'R2
model_summary$validation$R2

top_vars <- names(sort(coefficients, decreasing = TRUE))[1:20]

# Seleziona i coefficienti delle prime 20 variabili
top_coefficients <- coefficients[top_vars]

# Calcola il cambiamento percentuale
percent_change <- (exp(top_coefficients) - 1) * 100

# Stampa i risultati
print(percent_change)

percent_change<-(exp(coefficients)-1)*100













for (i in 1:10) {
  test_set[paste0("is_group_", i)] <- ifelse(test_set$Squad_Group == i, 1, 0)
}

test_set$Squad_Group<-NULL
test_set$Annual.Wages<-log(test_set$Annual.Wages)
test_set<-na.omit(test_set)
predictions_pcr_test <- exp((predict(fit_optimal_pcr, newdata = test_set, ncomp = 31)))
predictions_pcr_test
# Calcola l'MSE per il modello PCR
mse_pcr_test <- mean((predictions_pcr_test - exp(test_set$Annual.Wages))^2)
mse_pcr_test
Observed<-exp(test_set$Annual.Wages)

data <- data.frame(
  Observed = exp(test_set$Annual.Wages),
  Predicted_Stepwise = prediction_step_test,
  Predicted_PCR = predictions_pcr_test
)
library(ggplot2)
head(data)
# Suppongo che le previsioni e gli stipendi osservati siano in dataframes separati.
# Unisci i tre vettori in un singolo dataframe per la comodità della visualizzazione.



# Creazione del grafico
ggplot(data, aes(x = Observed)) +
  geom_histogram(aes(y = ..density..), bins = 30, fill = "light gray", color = "black") +
  geom_density(aes(y = ..density..), color = "green") +
  geom_density(aes(x = Annual.Wages.31.comps, y = ..density..), color = "red") +
  geom_density(aes(x = Predicted_Stepwise, y = ..density..), color = "blue")
  labs(x = "Salari",y = "Densità") +
  theme_minimal()


# Per il modello stepwise
ggplot(data, aes(x = Observed, y = Predicted_Stepwise)) +
  geom_point(color='red') +
  geom_smooth(method = "lm", color = "black") +
  xlab("Stipendi reali della stagione 2022/23") +
  ylab("Valori predetti dal modello Stepwise")

# Per il modello PCR
ggplot(data, aes(x = Observed, y = Annual.Wages.31.comps)) +
  geom_point(color='blue') +
  geom_smooth(method = "lm", color='black') +
  xlab("Stipendi reali della stagione 2022/23") +
  ylab("Valori predetti dal modello PCR")

head(data)
subset(test_set, exp(Annual.Wages) > quantile(exp(Annual.Wages), 0.99))
test_set <- subset(test_set, !(row.names(test_set) %in% row.names(outliers)))
test_set_standardized<-subset(test_set_standardized, !(row.names(test_set_standardized) %in% row.names(outliers)))





#----------------------------------------------------------------------------
#-----------------------------------------------------------------------------
#----------------------------------------------------------------------------
#PCA senza riferimento degli stipendi
#divisione del dataset per ruolo
dataset<-na.omit(dataset)

dataset_pca<-dataset[, -c(2,3)]
dataset_pca<-na.omit(dataset_pca)
dataset_FW<-dataset_pca[dataset_pca$Pos_FW==1, ]
dataset_MF<-dataset_pca[dataset_pca$Pos_MF==1, ]
dataset_DF<-dataset_pca[dataset_pca$Pos_DF==1, ]


#ora decido di applicare 
which(colnames(dataset_FW)=='Annual.Wages')
dataset_FW<-dataset_FW[, -c(83,84,85,86,87)]
dataset_MF<-dataset_MF[, -c(83,84,85,86,87)]
dataset_DF<-dataset_DF[, -c(83,84,85,86,87)]



#PCA
pca_forwards <- prcomp(dataset_FW, scale = TRUE)  # scale = TRUE centra e ridimensiona le variabili
pca_midfielders <- prcomp(dataset_MF, scale = TRUE)
pca_defenders <- prcomp(dataset_DF, scale = TRUE)

summary(pca_forwards)#19
summary(pca_midfielders)#19
summary(pca_defenders)#21


# 1. Centra i dati
data_centered_fw <- scale(dataset_FW, center = TRUE, scale = FALSE)
pca_forwards$rotation
pca_forwards$rotation[7,]<-pca_forwards$rotation[7,]*1000
# 2. Calcola i punteggi delle componenti principali usando la matrice dei loadings
manual_scores <- data_centered_fw %*% pca_forwards$rotation
manual_scores


# Grafico a barre dei carichi per la prima componente principale
barplot(loadings_matrix_FW[,1]*-1, main="Loadings for PC1", col="skyblue", las=2)

# Ottenere i punteggi delle componenti
scores <- pca_forwards$x
scores
# Creare un indice di prestazione sommando i punteggi delle prime 9 componenti
dataset_FW$PerformanceIndex <- rowSums(scores[,1:82])*-1

dataset_FW <- merge(dataset_FW, dataset[, c("Rk", "Player")], by="Rk")

# Creare un dataframe con i nomi dei giocatori e l'indice di prestazione
players_ranked <- data.frame(PlayerName=dataset_FW$Player, PerformanceIndex=dataset_FW$PerformanceIndex)

# Ordinare il dataframe in base all'indice di prestazione in ordine decrescente
players_ranked <- players_ranked[order(-players_ranked$PerformanceIndex),]

# Visualizzare i primi giocatori
head(players_ranked)


# Ottenere i punteggi delle componenti
scores_MF<- pca_midfielders$x

# Creare un indice di prestazione sommando i punteggi delle prime 9 componenti
dataset_MF$PerformanceIndex <- rowSums(scores_MF[,1:82])*1

dataset_MF <- merge(dataset_MF, dataset[, c("Rk", "Player")], by="Rk")

# Creare un dataframe con i nomi dei giocatori e l'indice di prestazione
players_ranked_MF <- data.frame(PlayerName=dataset_MF$Player, PerformanceIndex=dataset_MF$PerformanceIndex, RK=dataset_MF$Rk)

# Ordinare il dataframe in base all'indice di prestazione in ordine decrescente
players_ranked_MF <- players_ranked_MF[order(-players_ranked_MF$PerformanceIndex),]

# Visualizzare i primi giocatori
head(players_ranked_MF)



# Ottenere i punteggi delle componenti
scores_DF<- pca_defenders$x

# Creare un indice di prestazione sommando i punteggi delle prime 9 componenti
dataset_DF$PerformanceIndex <- rowSums(scores_DF[,1:82])*1

dataset_DF <- merge(dataset_DF, dataset[, c("Rk", "Player")], by="Rk")

# Creare un dataframe con i nomi dei giocatori e l'indice di prestazione
players_ranked_DF <- data.frame(PlayerName=dataset_DF$Player, PerformanceIndex=dataset_DF$PerformanceIndex)

# Ordinare il dataframe in base all'indice di prestazione in ordine decrescente
players_ranked_DF <- players_ranked_DF[order(-players_ranked_DF$PerformanceIndex),]

# Visualizzare i primi giocatori
head(players_ranked_DF)

combined_dataset <- rbind(players_ranked, players_ranked_DF)
combined_dataset<-rbind(combined_dataset, players_ranked_MF)
players_ranked_MF$RK<-NULL


combined_dataset <- combined_dataset[order(-combined_dataset$PerformanceIndex),]





