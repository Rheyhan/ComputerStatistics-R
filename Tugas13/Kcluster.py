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