#Load Packages
library(data.table)
library(stringr)
library(ggplot2)
library(ggsci)
library(cowplot)

#Load Data
AF <- read.csv("AF_auc_method.csv")
BC <- read.csv("BC_auc_method.csv")
CAD <- read.csv("CAD_auc_method.csv")
DIA <- read.csv("DIA_auc_method.csv")
LDL <- read.csv("LDL_R2_method.csv")
HDL <- read.csv("HDL_R2_method.csv")
TC <- read.csv("TC_R2_method.csv")
TG <- read.csv("TG_R2_method.csv")

#Plot
point <- geom_point(position = position_dodge(width = 0.5), size = 3) 
errorbar_B <- geom_errorbar(aes(ymin = auc_LL, ymax = auc_UL),
    width = 0.1,size=0.2, position = position_dodge(width = .5)) 
errorbar_Q <- geom_errorbar(aes(ymin = rsquared_LL, ymax = rsquared_UL),
    width = 0.1,size=0.2, position = position_dodge(width = .5)) 
mytheme <- theme_classic() + theme(
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
    
y_con_B <- scale_y_continuous(limits = c(0.4,0.8))
lab_name_B <- labs(x = "Ethnic", y = "AUC")
y_con_Q <- scale_y_continuous(limits = c(0,0.15))
lab_name_Q <- labs(x = "Ethnic", y = "Rsquared")

AF_plot <- ggplot(data = AF, aes(x = ethnic, col = method, shape = method, y = auc)) + 
    point + errorbar_B + mytheme + y_con_B + lab_name_B + scale_color_jama() +
    scale_shape_manual(values=c(15,15,15))

BC_plot <- ggplot(data = BC, aes(x = ethnic, col = method, shape = method, y = auc)) + 
    point + errorbar_B + mytheme + y_con_B + lab_name_B + scale_color_jama() +
    scale_shape_manual(values=c(15,15,15))

CAD_plot <- ggplot(data = CAD, aes(x = ethnic, col = method, shape = method, y = auc)) + 
    point + errorbar_B + mytheme + y_con_B + lab_name_B + scale_color_jama() +
    scale_shape_manual(values=c(15,15,15))

DIA_plot <- ggplot(data = DIA, aes(x = ethnic, col = method, shape = method, y = auc)) + 
    point + errorbar_B + mytheme + y_con_B + lab_name_B + scale_color_jama() +
    scale_shape_manual(values=c(15,15,15))

HDL_plot <- ggplot(data = HDL, aes(x = ethnic, col = method, shape = method, y = rsquared)) + 
    point + errorbar_Q + mytheme + y_con_Q + lab_name_Q + scale_color_jama() +
    scale_shape_manual(values=c(15,15,15))

LDL_plot <- ggplot(data = LDL, aes(x = ethnic, col = method, shape = method, y = rsquared)) + 
    point + errorbar_Q + mytheme + y_con_Q + lab_name_Q + scale_color_jama() +
    scale_shape_manual(values=c(15,15,15))
    
TC_plot <- ggplot(data = TC, aes(x = ethnic, col = method, shape = method, y = rsquared)) + 
    point + errorbar_Q + mytheme + y_con_Q + lab_name_Q + scale_color_jama() +
    scale_shape_manual(values=c(15,15,15))

TG_plot <- ggplot(data = TG, aes(x = ethnic, col = method, shape = method, y = rsquared)) +
    point + errorbar_Q + mytheme + y_con_Q + lab_name_Q + scale_color_jama() +
    scale_shape_manual(values=c(15,15,15))

pdf("figure/performance_of_prs.pdf", family = "Arial", height = 10,width = 8, onefile = FALSE)
ggarrange(
    AF_plot, BC_plot, CAD_plot, DIA_plot,
    HDL_plot, LDL_plot, TC_plot, TG_plot,
    ncol = 2, nrow = 4, hjust = -0.3, vjust = 1.6,
    font.label = list(size = 10, family="Arial"),
    labels = c("(A) AF", "(B) BC", "(C) CAD", "(D) DIA",
        "(E) HDL", "(F) LDL", "(G) TC", "(H) TG"),
    common.legend = TRUE) + theme(plot.margin = margin(10,10,10,10))
dev.off()
embedFonts(path.expand("figure/performance_of_prs.pdf"))
