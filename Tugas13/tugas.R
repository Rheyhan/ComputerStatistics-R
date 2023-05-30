#library
library(metan)
library(tidyr)
library(NbClust)
library(factoextra)
library(ggplot2)
library(cluster)
library(dplyr)
library(ggpubr)
library(olsrr)
library(spdep)
library(rgdal)
library(raster)
#initugas
#1-----------------------------------------------------
  #class making
setClass("BangunDatar")
setClass("Lingkaran", representation(jari_jari="numeric"),
         contains = "BangunDatar")

setClass("Poligon", representation(jumlah_sisi="integer"),
         contains = "BangunDatar")
setClass("Persegi", representation(panjang="numeric", 
                                   lebar="numeric"),
         contains = "Poligon")
setClass("Segitiga", representation(alas="numeric",
                                    tinggi="numeric"),
         contains = "Poligon")

  #assigning dummy
inisegitiga<-new("Segitiga", alas=5, tinggi=3, jumlah_sisi=as.integer(3))
inilingkaran<-new("Lingkaran", jari_jari=7)
inipersegi<-new("Persegi", panjang=12, lebar=6, jumlah_sisi=as.integer(4))
  #creating show() methode
setMethod("show", "Segitiga",
          function(object){
            cat("Bangun Ruang Segitiga dengan:\n  alas    =",object@alas,"\n  tinggi  =",object@tinggi)
          } 
)
setMethod("show", "Lingkaran",
          function(object){
            cat("Bangun Ruang Lingkaeran dengan:\n  jari-jari =",object@jari_jari)         
            } 
)
setMethod("show", "Persegi",
          function(object){
            cat("Bangun Ruang persegi dengan:\n  panjang  =",object@panjang,"\n  lebar    =",object@lebar)
          } 
)

  #displaying show() methode
show(inisegitiga)
show(inipersegi)
show(inilingkaran)



#2----------------------------------------------------
setwd("D:/Kuliah/Semester 4/Komputasi statistika/Tugas13")
df<-read.csv("Data2022.csv")
head(df,5)

  #eksplorasi
df[is.na(df)] = 0
df1<-data.frame(df[2:6],
                (df[7]+df[8])/2)
colnames(df1)[6] <- "pendudukmiskintot"
row.names(df1)<-df$Provinsi
plot(corr_coef(df1))  #Y= pengubah tetap, X= pengubah bebas

model.rlb<-lm(IPM~., data=df1) #y=ipm

ols_plot_resid_lev(model.rlb) #cek pencilan dan outlier
boxplot(df1$IPM)
  #geospatial ipm per prov
untukgeo<-read.csv("untukgeo.csv")
indonesia<-readOGR(dsn="D:/Kuliah/Semester 4/Komputasi statistika/Tugas13/SHP Indonesia", 
                   layer="prov")
plot(indonesia)
    #join
indonesia2 <- merge(indonesia, untukgeo, by="KODE", all.x = T)
    #visualisasi
k = 16
colfunc <- colorRampPalette(c("#ffbaba","red"))
color <- colfunc(k)

spplot(indonesia2, "IPM", col.regions=color)

#sisanya jelasin regresi dari bagian eksplorasi / k klustering (prov kaya, menengah, miskin) -> (clustered scatter & polar coordinates visualization setiap peubah)
scaleddf1<-scale(df1) #scale
  #finding the optimal cluster
    #silhouette
fviz_nbclust(scaleddf1, kmeans, method = "silhouette") #k=2
    #elbow
fviz_nbclust(scaleddf1, kmeans, method = "wss") #k= 2|4
    #gap stat
fviz_nbclust(x = scaleddf1, FUNcluster = kmeans, method = "gap_stat") #k=1
    #nbclust
nb<-NbClust(data = scaleddf1, distance = "euclidean", method="kmeans") #k=3

  #visualization
pam1 <- pam(scaleddf1, k=3, metric="euclidean") #using k=3
      #text
fviz_cluster(pam1, data=scaleddf1, geom="text", ellipse.type="norm", repel=TRUE, ggtheme=theme_bw()) +
  labs(title="Cluster plot", subtitle="k=3 clusters", x="Dim1, 20% variance", y="Dim2, 20% variance" )

      #check boxplot
clusterprovdata<-data.frame(scaleddf1,
                              cluster=as.factor(pam1$cluster))

a=scale_color_manual(values = c("1" = "blue", "2"="green", "3"="red")) 
plot1<- ggplot(clusterprovdata, aes(y = Pengangguran.terbuka, x = cluster, color=cluster)) + geom_boxplot(alpha=0.3)+a
plot2<-ggplot(clusterprovdata, aes(y = IPM, x = cluster, color=cluster)) + geom_boxplot(alpha=0.3)+a
plot3<-ggplot(clusterprovdata, aes(y = PDRB, x = cluster, color=cluster)) + geom_boxplot(alpha=0.3)+a
plot4<-ggplot(clusterprovdata, aes(y = Angka.harapan.hidup, x = cluster, color=cluster)) + geom_boxplot(alpha=0.3)+a
plot5<-ggplot(clusterprovdata, aes(y = Lama.sekolah, x = cluster, color=cluster)) + geom_boxplot(alpha=0.3)+a
plot6<-ggplot(clusterprovdata, aes(y = pendudukmiskintot, x = cluster, color=cluster)) + geom_boxplot(alpha=0.3) +a

ggarrange(plot1, plot2, plot3, plot4, plot5, plot6,
          ncol = 3, nrow = 2,  align = "hv", 
          common.legend = TRUE)

      #polar visualization
#python
        #import libraries
import pandas as pd
from sklearn.cluster import KMeans
from sklearn.preprocessing import MinMaxScaler
import plotly.express as px

        #dataframe
df1=pd.read_csv("D:/Kuliah/Semester 4/Komputasi statistika/Tugas13/asu.csv")
        #scaling
scale = MinMaxScaler()
df2=df1.drop(["Prov"], axis=1)
df = scale.fit_transform(df2)
df=pd.DataFrame(df)
axiscol=[(df2.axes)[1][i] for i in range(0, (df2.shape)[1])]   
df=df.set_axis(axiscol, axis='columns')
df=df.set_index(df1["Prov"])

        #visualize (polar)
kmeans = KMeans(n_clusters = 3, init = 'k-means++', n_init=10)
kmeans.fit(df)
df['label']=kmeans.labels_
polar=df.groupby("label").mean().reset_index()
polar=pd.melt(polar,id_vars=["label"])
fig4 = px.line_polar(polar, r="value", theta="variable", color="label", line_close=True,height=800,width=1400)
fig4.show()