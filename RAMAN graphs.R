setwd("C:/Masters/Thesis/RAMAN/data to graph")


library(ggplot2)

# Cotton - H brown fibre
H_Cotton <- read.delim("H1 R2 F1 brown fibre--Spectrum--023--Spec.Data 2 (CRR) (Sub BG).txt", header = FALSE, sep = "\t", dec = ".")

ggplot(data=H_Cotton, aes(x=V1, y=V2))+
  geom_line() +
  scale_x_continuous(limits = c(200, 1700)) +
  scale_y_continuous(limits = c(-10, 5000)) +
  labs(x = "rel. 1/cm", y = "CCD cts") +
  ggtitle("Cotton") # for the main title



#PTFE - H clear fibre
H_PTFE <- read.delim("H1 R2 F1 clear fibre--Spectrum--028--Spec.Data 2 (CRR) (Sub BG).txt", header = FALSE, sep = "\t", dec = ".")


ggplot(data=H_PTFE, aes(x=V1, y=V2))+
  geom_line() +
  scale_x_continuous(limits = c(200, 2000)) +
  scale_y_continuous(limits = c(-10, 2000)) +
  labs(x = "rel. 1/cm", y = "CCD cts") +
  ggtitle("PTFE") # for the main title

#PE - SC clear fibre
SC_PE <- read.delim("22.10.21.southcoolup_clearfibre3--Spectrum--041--Spec.Data 2 (Sub BG)DOWNLOAD.txt", header = FALSE, sep = "\t", dec = ".")

ggplot(data=SC_PE, aes(x=V1, y=V2))+
  geom_line() +
  scale_x_continuous(limits = c(1800, 3000)) +
  scale_y_continuous(limits = c(-10, 400)) +
  labs(x = "rel. 1/cm", y = "CCD cts") +
  ggtitle("PE") # for the main title

#PP - Kclear fibre
K_PP <- read.delim("25.10.clearfibre3--Spectrum--018--Spec.Data 1wide range (CRR) (Sub BG)DOWNLOAD.txt", header = FALSE, sep = "\t", dec = ".")

ggplot(data=K_PP, aes(x=V1, y=V2))+
  geom_line() +
  scale_x_continuous(limits = c(600, 3200)) +
  scale_y_continuous(limits = c(-10, 500)) +
  labs(x = "rel. 1/cm", y = "CCD cts") +
  ggtitle("PP") # for the main title

#Wool - K black 
K_Wool <- read.delim("25.10.K.blacfibre1--Spectrum--054--Spec.Data 1wide range (Average 5) (Sub BG)DOWNLOAD.txt", header = FALSE, sep = "\t", dec = ".")

ggplot(data=K_Wool, aes(x=V1, y=V2))+
  geom_line() +
  scale_x_continuous(limits = c(600, 3200)) +
  scale_y_continuous(limits = c(-10, 500)) +
  labs(x = "rel. 1/cm", y = "CCD cts") +
  ggtitle("Wool") # for the main title

#PVDF - SB black
SB_PVDF <- read.delim("25.10.SB blac fibre3--Spectrum--067--Spec.Data 1 (Average 5) (Sub BG)DOWNLOAD.txt", header = FALSE, sep = "\t", dec = ".")

ggplot(data=SB_PVDF, aes(x=V1, y=V2))+
  geom_line() +
  scale_x_continuous(limits = c(700, 3200)) +
  scale_y_continuous(limits = c(-1, 15)) +
  labs(x = "rel. 1/cm", y = "CCD cts") +
  ggtitle("PVDF") # for the main title

