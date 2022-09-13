# PACKAGES

{
library(rio)
library(tidyr)
library(tidyverse)
library(ggplot2)
library(dplyr)
library(glue)
library(plotly)
library(lubridate)
library("highcharter")
library(readxl)
}

# AFILIADOS

# Tidy

{
# 2019

ii <- "01"

df <- read_excel(path = glue("./datos/afiliados/afiliados2019" , {ii} , ".xlsx") , sheet = 1 , col_names = TRUE , skip = 1)
df <- df %>% filter(MUNICIPIO %in% c("46250 VALENCIA" , "46250 VALENCIA/VALÈNCIA")) %>%
  mutate(year = 2019 , mes = "01" , muni = "Valencia") %>%
  select(year , mes , muni , total = TOTAL)

x <- c("02" , "03" , "04" , "05" , "06" , "07" , "08" , "09" , "10" , "11" , "12")

for(ii in x) {
          df1 <- read_excel(path = glue("./datos/afiliados/afiliados2019" , {ii} , ".xlsx") , sheet = 1 , col_names = TRUE , skip = 1)
          df1 <- df1 %>% filter(MUNICIPIO %in% c("46250 VALENCIA" , "46250 VALENCIA/VALÈNCIA")) %>%
            mutate(year = 2019 , mes = {ii} , muni = "Valencia") %>%
            select(year , mes , muni , total = TOTAL)
          df <- bind_rows(df , df1)
        }

# 2020

x <- c("01" , "02" , "03")

for(ii in x) {
          df1 <- read_excel(path = glue("./datos/afiliados/afiliados2020" , {ii} , ".xlsx") , sheet = 1 , col_names = TRUE , skip = 1)
          df1 <- df1 %>% filter(MUNICIPIO %in% c("46250 VALENCIA" , "46250 VALENCIA/VALÈNCIA")) %>%
            mutate(year = 2020 , mes = {ii} , muni = "Valencia") %>%
            select(year , mes , muni , total = TOTAL)
          df <- bind_rows(df , df1)
          }

df <- df %>% mutate(dia = gsub(x = mes , "01" , replacement = "31")) %>%
    mutate(dia = gsub(x = dia , "02" , replacement = "28")) %>%
    mutate(dia = gsub(x = dia , "03" , replacement = "31")) %>%
    mutate(dia = gsub(x = dia , "04" , replacement = "30")) %>%
    mutate(dia = gsub(x = dia , "05" , replacement = "31")) %>%
    mutate(dia = gsub(x = dia , "06" , replacement = "30")) %>%
    mutate(dia = gsub(x = dia , "07" , replacement = "31")) %>%
    mutate(dia = gsub(x = dia , "08" , replacement = "31")) %>%
    mutate(dia = gsub(x = dia , "09" , replacement = "30")) %>%
    mutate(dia = gsub(x = dia , "10" , replacement = "31")) %>%
    mutate(dia = gsub(x = dia , "11" , replacement = "30")) %>%
    mutate(dia = gsub(x = dia , "12" , replacement = "31")) %>%
    mutate(dia = as.numeric(dia))

df <- df %>% mutate(fecha = paste(year , mes , dia , sep = "-" , collapse = NULL)) %>%
  mutate(fecha = ymd(fecha)) %>%
  select(fecha , muni , total)
zz <- df %>% filter(fecha == last(fecha , order_by = fecha))
zz <- as.numeric(zz$total)
zz <- format(zz , decimal.mark = "," , big.mark=".")
}

# Plot (con titulo)
{
hchart(df, "line", hcaes(x = fecha, y = as.numeric(total))) %>%
  hc_xAxis(title = list(text = "Mes")) %>%
  hc_yAxis(title = list(text = "Nombre d'afiliats")) %>%
  hc_add_theme(hc_theme_gridlight(colors = c("firebrick" , "orange" , "yellow"))) %>%
  hc_title(text = "Afiliats a la Seguretat Social de València <br><br> Evolució mensual" , align = "left" , style = list(color = "firebrick" , fontSize = "1.4em") , useHTML = TRUE) %>%
  hc_subtitle(text = zz , align = "right" , style = list(color = "firebrick" , fontSize = "1.4em" , fontweight = "bold") , useHTML = TRUE , y = 80) %>%
  hc_credits(enabled = TRUE , text = "<i>Font: Seguridad Social</i>" , href = "http://www.seg-social.es/wps/portal/wss/internet/Inicio" , style = list(fontSize = "1em" , color = "black") , useHTML = TRUE)
}

# Plot (sin titulo)
{
hchart(df, "line", hcaes(x = fecha, y = as.numeric(total))) %>%
  hc_xAxis(title = list(text = "Mes")) %>%
  hc_yAxis(title = list(text = "Nombre d'afiliats")) %>%
  hc_add_theme(hc_theme_gridlight(colors = c("firebrick" , "orange" , "yellow"))) %>%
  hc_credits(enabled = TRUE , text = "<i>Font: Seguridad Social</i>" , href = "http://www.seg-social.es/wps/portal/wss/internet/Inicio" , style = list(fontSize = "1em" , color = "black") , useHTML = TRUE)
}

hchart(df, "line", hcaes(x = fecha, y = as.numeric(total) , group = muni) , name = c("Valencia")) %>%
  hc_xAxis(title = list(text = "Mes")) %>%
  hc_yAxis(title = list(text = "Nombre d'afiliats")) %>%
  hc_add_theme(hc_theme_538(colors = c("red", "blue", "green") , chart = list(backgroundColor = "white"))) %>%
  hc_title(text = "This is a title with <i>margin</i> and <b>Strong or bold text</b>",margin = 20, align = "left",
           style = list(color = "#90ed7d", useHTML = TRUE)) %>%
  hc_subtitle(text = "And this is a subtitle with more information",
              align = "left", style = list(color = "#2b908f", fontWeight = "bold"))

# PARADOS
{
# 2019

x <- c("01" , "02" , "03" , "04" , "05" , "06" , "07" , "08" , "09" , "10" , "11" , "12")

for(ii in x) {
          df <- read_excel(path = glue("./datos/parados/parados2019" , {ii} , ".xlsx") , sheet = 1 , col_names = TRUE , skip = 1)
          assign(x = paste0("parados19" , {ii}) , value = df)
        }

# 2020

x <- c("01" , "02" , "03" , "04")

for(ii in x) {
          df <- read_excel(path = glue("./datos/parados/parados2020" , {ii} , ".xlsx") , sheet = 1 , col_names = TRUE , skip = 1)
          assign(x = paste0("parados20" , {ii}) , value = df)
        }
}

df <- read_excel(path = "./datos/parados/parados201901.xlsx" , sheet = 1 , col_names = TRUE , skip = 1)

# CONTRATOS
{
# 2019

x <- c("01" , "02" , "03" , "04" , "05" , "06" , "07" , "08" , "09" , "10" , "11" , "12")

for(ii in x) {
          df <- read_excel(path = glue("./datos/contratos/contratos2019" , {ii} , ".xlsx") , sheet = 1 , col_names = TRUE , skip = 1)
          assign(x = paste0("contratos19" , {ii}) , value = df)
        }

# 2020

x <- c("01" , "02" , "03")

for(ii in x) {
          df <- read_excel(path = glue("./datos/afiliados/afiliados2020" , {ii} , ".xlsx") , sheet = 1 , col_names = TRUE , skip = 1)
          assign(x = paste0("contratos20" , {ii}) , value = df)
        }
}










# COVID-19

{
#NEW
{
    x <- c("confirmados_pcr" , "confirmados_test" , "fallecidos" , "hospitalizados" , "positivos_asintomaticos" , "uci")

{
    ii <- "altas"

url <- glue("https://raw.githubusercontent.com/datadista/datasets/master/COVID%2019/" , "ccaa_covid19_" , {ii} , "_long" , ".csv")

    download.file(url , destfile = glue("./datos/" , {ii} , ".csv"))

    df <- read.csv(glue("./datos/" , {ii} , ".csv"),
                        header=TRUE,
                        encoding="UTF-8",
                        stringsAsFactors=FALSE
                      )
    df <- df %>% filter(CCAA == "C. Valenciana")

    df <- setNames(object = df , nm = c("fecha" , "cod_ine" , "CCAA" , {ii}))
}


for (ii in x) {
    url <- glue("https://raw.githubusercontent.com/datadista/datasets/master/COVID%2019/" , "ccaa_covid19_" , {ii} , "_long" , ".csv")

    download.file(url , destfile = glue("./datos/" , {ii} , ".csv"))

    df1 <- read.csv(glue("./datos/" , {ii} , ".csv"),
                        header=TRUE,
                        encoding="UTF-8",
                        stringsAsFactors=FALSE
                      )
    df1 <- df1 %>% filter(CCAA == "C. Valenciana")
    assign(value = df1 , x = paste0("df" , ii))

    df1 <- setNames(object = df1 , nm = c("fecha" , "cod_ine" , "CCAA" , {ii}))

    df <- left_join(df , df1 , by =c("fecha" , "cod_ine" , "CCAA"))
}


df <- df %>% mutate(fecha = ymd(fecha))
df1 <- df %>% select(Fecha = fecha , Altas = altas , Contagiados = confirmados_pcr , Exitus = fallecidos , UCI = uci) %>%
    pivot_longer(cols = 2:5 , names_to = "Situación" , values_to = "Casos")

df1 <- highlight_key(df1 , ~Situación)
p <- ggplot(df1 , aes(x = Fecha , y = Casos , color = Situación)) +
  geom_line() +
  geom_point() +
  labs(title = glue("Casos de Covid19 en Com. Valenciana") , x = "Día"  , y = "Número de casos" , color = "Edad") +
  scale_colour_brewer(palette = "YlOrRd") +
  theme(axis.text.y = element_text(angle = 45, hjust = 1) ,
    axis.text.x = element_text(angle = 45, hjust = 1) ,
    panel.background = element_rect(fill = "slategray4", colour = "snow4") ,
    plot.background = element_rect(fill = "gray91" , colour = NA, size = 0) ,
    legend.key = element_rect(fill = "gray91"),
    legend.background = element_rect(fill = "gray91") ,
    plot.title = element_text(size = 17, face = "bold") ,
    axis.title = element_text(face = "bold"),
    axis.text = element_text(face = "bold"))
p <- ggplotly(p , tooltip = c("Fecha" ,  "Casos" , "Situación"))

highlight(p, dynamic = TRUE , on = "plotly_hover" , off = 'plotly_relayout')
}


#OLD
{
    x <- c("confirmados_pcr" , "confirmados_test" , "fallecidos" , "hospitalizados" , "positivos_asintomaticos" , "uci")


{
    ii <- "altas"

url <- glue("https://raw.githubusercontent.com/datadista/datasets/master/COVID%2019/old_series/" , "ccaa_covid19_" , {ii} , "_long_old" , ".csv")

    download.file(url , destfile = glue("./datos/" , {ii} , "_old" , ".csv"))

    df <- read.csv(glue("./datos/" , {ii} , ".csv"),
                        header=TRUE,
                        encoding="UTF-8",
                        stringsAsFactors=FALSE
                      )
    df <- df %>% filter(CCAA == "C. Valenciana")
    df <- setNames(object = df , nm = c("fecha" , "cod_ine" , "CCAA" , {ii}))
}


for (ii in x) {
    url <- glue("https://raw.githubusercontent.com/datadista/datasets/master/COVID%2019/old_series/" , "ccaa_covid19_" , {ii} , "_long_old" , ".csv")

    download.file(url , destfile = glue("./datos/" , {ii} , "_old" , ".csv"))

    df1 <- read.csv(glue("./datos/" , {ii} , ".csv"),
                        header=TRUE,
                        encoding="UTF-8",
                        stringsAsFactors=FALSE
                      )
    df1 <- df1 %>% filter(CCAA == "C. Valenciana")
    assign(value = df1 , x = paste0("df" , ii))

    df1 <- setNames(object = df1 , nm = c("fecha" , "cod_ine" , "CCAA" , {ii}))

    df <- left_join(df , df1 , by =c("fecha" , "cod_ine" , "CCAA"))
}


df <- df %>% mutate(fecha = ymd(fecha))
df1 <- df %>% select(Fecha = fecha , Altas = altas , Contagiados = confirmados_pcr , Exitus = fallecidos , UCI = uci) %>%
    pivot_longer(cols = 2:5 , names_to = "Situación" , values_to = "Casos")

df1 <- highlight_key(df1 , ~Situación)
p <- ggplot(df1 , aes(x = Fecha , y = Casos , color = Situación)) +
  geom_line() +
  geom_point() +
  labs(title = glue("Casos de Covid19 en Com. Valenciana") , x = "Día"  , y = "Número de casos" , color = "Edad") +
  scale_colour_brewer(palette = "YlOrRd") +
  theme(axis.text.y = element_text(angle = 45, hjust = 1) ,
    axis.text.x = element_text(angle = 45, hjust = 1) ,
    panel.background = element_rect(fill = "slategray4", colour = "snow4") ,
    plot.background = element_rect(fill = "gray91" , colour = NA, size = 0) ,
    legend.key = element_rect(fill = "gray91"),
    legend.background = element_rect(fill = "gray91") ,
    plot.title = element_text(size = 17, face = "bold") ,
    axis.title = element_text(face = "bold"),
    axis.text = element_text(face = "bold"))
p <- ggplotly(p , tooltip = c("Fecha" ,  "Casos" , "Situación"))

highlight(p, dynamic = TRUE , on = "plotly_hover" , off = 'plotly_relayout')
}

df2 <- df %>% select(fecha , altas , confirmados_pcr , fallecidos , uci)

zz <- df2 %>% pivot_longer(cols = 2:5 , names_to = "variable" , values_to = "value")

hchart(zz, "line", hcaes(x = fecha, y = value, group = variable) , name = c("Altas" , "Contagiados" , "Exitus" , "UCI")) %>%
  hc_add_theme(hc_theme_google()) %>%
  hc_title(text = "A highcharter chart")
}
