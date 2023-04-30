# Cargamos las librerias con las que vamos a trabajar:

library(tidyverse)
library("janitor")
library(dplyr)
library(scales)
library(lubridate)
library(splines)
library(ggthemes)
library(extrafont)

############################################################ CARGO DATOS ########################################################


# Contabilidad mensual entre los años 2017 y 2019:

contabilidad_mensual <- read.csv("C:/Users/regue/Desktop/GOOGLE ANALYTICS/Final project/Online_Retailer_analysis/Data/contabilidad.csv")

# Venta de productos online entre los años 2017 y 2019:

venta_productos <- read.csv("C:/Users/regue/Desktop/GOOGLE ANALYTICS/Final project/Online_Retailer_analysis/Data/ventas.csv")

#################################################################################################################################


# Empezamos visualizando el tipo de datos que tenemos en el archivo "venta_productos":

head(venta_productos)
glimpse(venta_productos)
str(venta_productos)

View(venta_productos)

############################################################### LIMPIEZA DE DATOS ###############################################


# Compruebo si hay valores NA en el dataset

any(is.na(venta_productos))

# Parece haber una categoría vacía y se encuentra a su vez valores negativos en la fila 1767 , 
# limpiemos nuestros datos con el paquete dplyr:

venta_productos_clean <- venta_productos %>% 
  filter(Product.Type != "", Total.Net.Sales >= 0) %>% 
  arrange(desc(Net.Quantity))


########################################################## ANÁLISIS DE DATOS ####################################################

# Utilizamos SUMMARIZE para investigar el número total de produtos vendidos de cada tipo:

resumen_ventas <- venta_productos_clean %>% 
  group_by(Product.Type) %>% 
  summarize(total_vendidos = sum(Net.Quantity), total_descuentos = sum(Discounts), total_ganancias = sum(Total.Net.Sales)) %>%
  mutate(porcentaje = total_ganancias * 100 / sum(total_ganancias)) %>% 
  arrange(desc(total_ganancias))

# FILTRO Y SELECCIONO TIPOS DE PRODUCTOS:

producto_relevante <- resumen_ventas %>% 
  select(Product.Type,total_vendidos, total_ganancias, porcentaje) %>% 
  filter(porcentaje > 4)

producto_menos_relevante <- resumen_ventas %>% 
  select(Product.Type, total_vendidos, total_ganancias, porcentaje) %>%
  filter(porcentaje < 4)


producto_menos_relevante_resumen <- data.frame(Product.Type = "Others",total_vendidos = sum(producto_menos_relevante$total_vendidos), total_ganancias = sum(producto_menos_relevante$total_ganancias), porcentaje = sum(producto_menos_relevante$porcentaje))
  

# JUNTO DATOS:


resumen_ventas_filtrada <- rbind(producto_relevante, producto_menos_relevante_resumen)

# DATAFRAMES  

View(venta_productos_clean)

head(resumen_ventas)
View(resumen_ventas)

View(producto_relevante)
View(producto_menos_relevante_resumen)


########################### DATAFRAME FILTRADO###########################

View(resumen_ventas_filtrada)



##################################################### VISUALIZACIÓN DE DATOS ####################################################


######################################### Veamos ahora que da mayor número de beneficios netos: #################################

ggplot(resumen_ventas_filtrada) +
  geom_bar(mapping = aes(x = Product.Type, y = total_ganancias, fill = Product.Type), stat = "identity") +
  labs(title = "Beneficios netos",
       subtitle = "Entre los años 2017 y 2019",
       x = "Categoría",
       y = "Beneficios netos",
       fill = "Categoría") +
  scale_fill_discrete(labels = c("Escultura y Arte", "Baloncesto", "Navidad", "Decoración Hogar", "Joyería", "Cocina", "Otros")) +
  scale_y_continuous(labels = scales::dollar, breaks = seq(0, 250000, by = 20000))+
  scale_x_discrete(labels = c("Escultura y Arte", "Baloncesto", "Navidad", "Decoración Hogar", "Joyería", "Cocina", "Otros")) +
  theme_gdocs() +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1, size = 8))

############################################################## PIE CHART ########################################################

ggplot(resumen_ventas_filtrada, aes(x = "", y = total_ganancias , fill = Product.Type)) +
  geom_col(width = 1, color = "black", size = 0.25) +
  coord_polar("y", start=0) +
  theme_void() +
  geom_label(aes(label = paste0(round(porcentaje), "%")), position = position_stack(vjust = 0.5)) +
  labs(title = "Distribución de ingresos netos", fill = "Tipo de producto") +
  scale_fill_discrete(labels = c("Escultura y Arte", "Baloncesto", "Navidad", "Decoración Hogar", "Joyería", "Cocina", "Otros")) +
  guides(fill = guide_legend(title = "Categoría", override.aes = aes(label = "")))



#################################### Visualicemos en un gráfico de barras los productos más vendidos: ###########################

ggplot(resumen_ventas_filtrada) + 
  geom_bar(mapping = aes(x=Product.Type, y = total_vendidos, fill = Product.Type),stat = "identity") + 
  labs(title = "Productos vendidos",
       subtitle = "Entre los años 2017 y 2019",
       x = "Categoría",
       y = "Cantidad",
       fill = "Categoría") + 
  scale_y_continuous(breaks = seq(0, 2500, by = 500))+
  scale_x_discrete(labels = c("Escultura y Arte", "Baloncesto", "Navidad", "Decoración Hogar", "Joyería", "Cocina", "Otros")) +
  scale_fill_discrete(labels = c("Escultura y Arte", "Baloncesto", "Navidad", "Decoración Hogar", "Joyería", "Cocina", "Otros")) +
  theme_gdocs() +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1, size = 8))



###################################################################################################################################################



# Empezamos visualizando el tipo de datos que tenemos en el archivo "contabilidad_mensual":

head(contabilidad_mensual)
glimpse(contabilidad_mensual)
str(contabilidad_mensual)

View(contabilidad_mensual)

############################################################### LIMPIEZA DE DATOS ###############################################


# Compruebo si hay valores NA en el dataset

any(is.na(contabilidad_mensual))
# Primero juntaremos Year y Month en la columna Date.
# Vamos a convertir la columna Date a formato fecha y ordenarlo con arrange:

contabilidad_mensual <- contabilidad_mensual %>% 
  mutate(Date = ymd(paste0(Year, Month, "-01")))

contabilidad_mensual_clean <- contabilidad_mensual %>% 
  select(Date, Total.Orders, Net.Sales, Total.Sales, Year, Month) %>% 
  rename(Año = Year, Mes = Month)




########################################################## ANÁLISIS DE DATOS ####################################################

resumen_ganancias <- contabilidad_mensual_clean %>% 
  group_by(Año) %>% 
  summarise(Ganancias_Netas = sum(Net.Sales))


resumen_pedidos <- contabilidad_mensual_clean %>% 
  group_by(Año) %>% 
  summarise(total_pedidos_mes = sum(Total.Orders))

# Convierto columna Año a factor:

contabilidad_mensual_clean$Año <- format(contabilidad_mensual_clean$Año)

resumen_ganancias$Año <- factor(resumen_ganancias$Año)

#Vector de colores:
colores <- c("#72D9FF", "#3FA0FF", "#290AD8")

# DATAFRAMES  

str(contabilidad_mensual_clean)

View(contabilidad_mensual_clean)

########################### DATAFRAME FILTRADO###########################

View(resumen_ganancias)
View(resumen_pedidos)

##################################################### VISUALIZACIÓN DE DATOS ####################################################


# Visualicemos cual ha sido la tendencia a lo largo de los años 2017, 2018, 2019:

ggplot(contabilidad_mensual_clean) +
  geom_area(mapping = aes(x = Date, y = Net.Sales, fill = Año), stat = "identity") +
  facet_wrap(~Año, scales = "free_x") + 
  theme(axis.text.x = element_text(angle = 45, vjust = 0.5, size = 6)) +
  scale_x_date(date_breaks = "3 month", date_labels = "%b") +
  scale_y_continuous(labels = scales::dollar, breaks = seq(0, 45000, by = 5000)) +
  scale_fill_manual(values = colores) +
  labs(title = "Ganancias netas",
       subtitle = "Entre los años 2017 y 2019",
       x = "Mes",
       y = "Cantidad total") +
  theme_gdocs()

### TENDENCIA ###
ggplot(contabilidad_mensual_clean) +
  geom_point(mapping = aes(x = Date, y = Net.Sales, color = Año), stat = "identity") + 
  geom_smooth(mapping = aes(x = Date, y = Net.Sales), method = "lm", se = FALSE, color = "#3C8625") +
  theme(axis.text.x = element_text(angle = 45, vjust = 0.5, size = 6)) +
  scale_x_date(date_breaks = "3 month", date_labels = "%b") +
  scale_y_continuous(labels = scales::dollar, breaks = seq(0, 45000, by = 5000)) +
  scale_color_manual(values = colores) +
  labs(title = "Ganancias netas",
       subtitle = "Entre los años 2017 y 2019",
       x = "Mes",
       y = "Cantidad total") +
  theme_gdocs()


# Visualicemos ahoar el número de pedidos realizados al mes durante los años 2017, 2018 y 2019:

rect_data <- data.frame(Año = c("2017", "2018", "2019"),
                        rect_xmin = as.Date(c("2017-10-01", "2018-10-01", "2019-10-01")),
                        rect_xmax = as.Date(c("2018-01-01", "2019-01-01", "2020-01-01")),
                        rect_ymin = c(0, 0, 0),
                        rect_ymax = c(120, 170, 350))


ggplot(contabilidad_mensual_clean) +
  geom_bar(mapping = aes(x = Date, y = Total.Orders, fill = Año), stat = "identity") +
  facet_wrap(~Año, scales = "free_x") + 
  theme(axis.text.x = element_text(angle = 45, vjust = 0.5, size = 6)) +
  scale_x_date(date_breaks = "3 month", date_labels = "%b") +
  scale_y_continuous(breaks = seq(0, 500, by = 100)) +
  scale_fill_manual(values = colores) +
  labs(title = "Número de pedidos", x = "Mes", y = "Cantidad") +
  geom_rect(data = rect_data,
            mapping = aes(xmin = rect_xmin, xmax = rect_xmax, ymin = rect_ymin, ymax = rect_ymax, fill = Año),
            alpha = 0.2,
            color = "red",
            inherit.aes = FALSE,
            size = 1,
            show.legend = FALSE) +
  theme_gdocs()

### TENDENCIA ###
ggplot(contabilidad_mensual_clean) +
  geom_point(mapping = aes(x = Date, y = Total.Orders, color = Año), stat = "identity") + 
  theme(axis.text.x = element_text(angle = 45, vjust = 0.5, size = 6)) +
  scale_x_date(date_breaks = "3 month", date_labels = "%b") +
  scale_y_continuous(breaks = seq(0, 500, by = 100)) +
  scale_color_manual(values = colores) +
  labs(title = "Número de pedidos", x = "Mes", y = "Cantidad") +
  theme_gdocs()


# Visualicemos las ganancias netas para cada uno de los años:

ggplot(resumen_ganancias) +
  geom_bar(mapping = aes(x = Año, y = Ganancias_Netas, fill = Año), stat = "identity") + 
  theme(axis.text.x = element_text(angle = 0, vjust = 0.5, size = 10)) +
  scale_y_continuous(labels = scales::dollar, breaks = seq(0, 300000, by = 40000)) +
  scale_fill_manual(values = colores) +
  labs(title = "Ganancias netas",
       subtitle = "Entre 2017 y 2019",
       x = "Año",
       y = "Cantidad Total") +
  theme_gdocs()


