#Methods improvement
#Load Packages
library(data.table)
library(stringr)
library(ggplot2)
library(ggsci)
library(cowplot)

#Load Data
AF <- read.csv("AF_auc_improve.csv")
BC <- read.csv("BC_auc_improve.csv")
CAD <- read.csv("CAD_auc_improve.csv")
DIA <- read.csv("DIA_auc_improve.csv")
LDL <- read.csv("LDL_R2_improve.csv")
HDL <- read.csv("HDL_R2_improve.csv")
TC <- read.csv("TC_R2_improve.csv")
TG <- read.csv("TG_R2_improve.csv")

#Plot
mytheme <- theme(
    panel.background = element_rect(fill = "white"),
    panel.grid.major.y = element_line(color = "darkgrey", size = 0.2),
    panel.grid.minor.y = element_blank(),
    panel.grid.minor.x = element_blank(),
    legend.position = "top",
    legend.direction = "horizontal",
    legend.title = element_text(size = 10, family = "Arial", face = "plain"),
    legend.text = element_text(hjust = 0,size = 10, family = "Arial", face = "plain"),
    legend.background = element_rect(size = 0.2, linetype = "solid",colour = "black"),
    axis.title = element_text(size = 10, family = "Arial", face = "plain"),
    axis.text.x = element_text(size = 10, family = "Arial", face = "plain"),
    axis.text.y = element_text(size = 10, family = "Arial", face = "plain"),
    plot.margin = margin(20,10,5,10))

bar_B <- geom_bar(stat = "identity", position = "dodge")
y_con_B <- scale_y_continuous(limits = c(-0.05,0.1))
lab_name_B <- labs(x = "Population", y = "AUC improve pct")
bar_Q <- geom_bar(stat="identity", position = "dodge")
y_con_Q <- scale_y_continuous(limits = c(-0.1,0.45))
lab_name_Q<-labs(x = "Population", y = "Rsquared improve pct")

AF_plot <- ggplot(data = AF, aes(x = Population, fill = Population, y = Improve)) + 
    bar_B + mytheme + y_con_B + lab_name_B + scale_fill_jama()

BC_plot <- ggplot(data = BC, aes(x = Population, fill = Population, y = Improve)) + 
    bar_B + mytheme + y_con_B + lab_name_B + scale_fill_jama()

CAD_plot <- ggplot(data = CAD, aes(x = Population, fill = Population, y = Improve)) + 
    bar_B + mytheme + y_con_B + lab_name_B + scale_fill_jama()

DIA_plot <- ggplot(data = DIA, aes(x = Population, fill = Population, y = Improve)) + 
    bar_B + mytheme + y_con_B + lab_name_B + scale_fill_jama()

HDL_plot <- ggplot(data = HDL, aes(x = Population, fill = Population, y = Improve)) + 
    bar_Q + mytheme + y_con_Q + lab_name_Q + scale_fill_jama()

LDL_plot <- ggplot(data = LDL, aes(x = Population, fill = Population, y = Improve)) + 
    bar_Q + mytheme + y_con_Q + lab_name_Q + scale_fill_jama()

TC_plot <- ggplot(data = TC, aes(x = Population, fill = Population, y = Improve)) + 
    bar_Q + mytheme + y_con_Q + lab_name_Q + scale_fill_jama()

TG_plot <- ggplot(data = TG, aes(x = Population, fill = Population, y = Improve)) + 
    bar_Q + mytheme + y_con_Q + lab_name_Q + scale_fill_jama()

pdf("figure/prediction_improvement_by_integrating_annotations.pdf",
    family="Arial",height=10,width = 8,onefile=FALSE)
ggarrange(
    AF_plot, BC_plot, CAD_plot, DIA_plot,
    HDL_plot, LDL_plot, TC_plot, TG_plot,
    ncol = 2, nrow = 4, hjust = -0.3, vjust = 1.6,
    font.label = list(size = 10, family="Arial"),
    labels = c("(A) AF", "(B) BC", "(C) CAD", "(D) DIA",
        "(E) HDL", "(F) LDL", "(G) TC", "(H) TG"),
    common.legend = TRUE) + theme(plot.margin = margin(10,10,10,10))
dev.off()
embedFonts(path.expand("figure/prediction_improvement_by_integrating_annotations.pdf"))

