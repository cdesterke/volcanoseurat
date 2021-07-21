library(dplyr)
markers <- FindAllMarkers(mono, only.pos = F, min.pct = 0.5, logfc.threshold = 0.25)
markers %>%
  group_by(cluster) %>%
  top_n(n = 10, wt = avg_log2FC) -> top10
top10



## volseurat(markers,nb=50,fc=0.5,p=0.05,size=2.5,alpha=0.3)

markers<-as_tibble(markers)
markers%>%filter(.,cluster=="NT4t1IPS")->markers


volseurat<-function(res,nb=30,fc=0.1,p=0.001,size=5,alpha=1,title=""){
  # require libraries
  if(!require(ggthemes)){install.packages("ggthemes")}
  library(ggthemes)
  if(!require(ggplot2)){install.packages("ggplot2")}
  library(ggplot2)
  if(!require(ggrepel)){install.packages("ggrepel")}
  library(ggrepel) 
  
  # add a column of NO UP DOWN
  res$diffexpressed <- "NO"
  res$diffexpressed[res$avg_log2FC> fc & res$p_val_adj < p] <- "UP"
  res$diffexpressed[res$avg_log2FC< -fc & res$p_val_adj < p] <- "DOWN"
  
          
  
  # perform the graph
  ggplot(data=res, aes(x=avg_log2FC, y=-log10(p_val_adj), col=diffexpressed, label=gene, size=pct.1)) +
    geom_point(alpha=alpha) + 
    theme_gdocs() +
    geom_text_repel(data=head(res, nb), aes(label=gene),size=size) +
    scale_color_manual(values=c("#00CCFF", "darkgray", "#FF00FF")) +
    geom_vline(xintercept=c(-fc, fc), col="orange",linetype="dashed") +
    geom_hline(yintercept=-log10(p), col="orange",linetype="dashed") +
    theme(legend.position = "none")+
    ggtitle(title)
  
}


volseurat(markers,nb=100,fc=0.1,p=0.05,size=4,alpha=0.3,title="Cluster 2 / CD14+ / vaccine vs control")
