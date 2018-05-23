#載入package
library(readr)
library(dplyr)
library(ggplot2)
library(rgdal)
library(rgeos)
library(maptools)
library(RColorBrewer)
library(jsonlite)

#讀取資料
#大專校院境外學生人數統計
overseasStudentCountry103<-read_csv("http://stats.moe.gov.tw/files/detail/103/103_ab103_C.csv")
overseasStudentCountry104<-read_csv("http://stats.moe.gov.tw/files/detail/104/104_ab104_C.csv")
overseasStudentCountry105<-read_csv("http://stats.moe.gov.tw/files/detail/105/105_ab105_C.csv")
overseasStudentCountry106<-read_csv("http://stats.moe.gov.tw/files/detail/106/106_ab105_C.csv")
overseasStudentSchool103<-read_csv("http://stats.moe.gov.tw/files/detail/103/103_ab103_S.csv")
overseasStudentSchool104<-read_csv("http://stats.moe.gov.tw/files/detail/104/104_ab104_S.csv")
overseasStudentSchool105<-read_csv("http://stats.moe.gov.tw/files/detail/105/105_ab105_S.csv")
overseasStudentSchool106<-read_csv("http://stats.moe.gov.tw/files/detail/106/106_ab105_S.csv")
#世界各主要國家之我國留學生人數統計表
country<-read_csv("https://ws.moe.edu.tw/Download.ashx?u=C099358C81D4876CC7586B178A6BD6D5062C39FB76BDE7EC7685C1A3C0846BCDD2B4F4C2FE907C3E7E96F97D24487065577A728C59D4D9A4ECDFF432EA5A114C8B01E4AFECC637696DE4DAECA03BB417&n=4E402A02CE6F0B6C1B3C7E89FDA1FAD0B5DDFA6F3DA74E2DA06AE927F09433CFBC07A1910C169A1845D8EB78BD7D60D7414F74617F2A6B71DC86D17C9DA3781394EF5794EEA7363C&icon=..csv")

#第一題_前
#欄位名稱處理
ColName<-colnames(overseasStudentCountry103)
ColName<-gsub("-","_",ColName)
colnames(overseasStudentCountry103)<-ColName
colnames(overseasStudentCountry104)<-ColName
#表格合併
overseasStudentCountry<-rbind(overseasStudentCountry103,
                              overseasStudentCountry104,
                              overseasStudentCountry105,
                              overseasStudentCountry106)

ToTWNCountry<-overseasStudentCountry%>%
  mutate(總數=學位生_正式修讀學位外國生+
              `學位生_僑生(含港澳)`+
              學位生_正式修讀學位陸生+
              非學位生_外國交換生+
              非學位生_外國短期研習及個人選讀+
              非學位生_大專附設華語文中心學生+
              非學位生_大陸研修生+
              非學位生_海青班+
              境外專班)%>%
  group_by(國別)%>%
  summarise(總人數=sum(總數))%>%
  arrange(desc(總人數))

#第一題_後
#欄位名稱處理
ColName<-colnames(overseasStudentSchool103)
ColName<-gsub("-","_",ColName)
colnames(overseasStudentSchool103)<-ColName
colnames(overseasStudentSchool104)<-ColName
#表格合併
overseasStudentSchool<-rbind(overseasStudentSchool103,
                             overseasStudentSchool104,
                             overseasStudentSchool105,
                             overseasStudentSchool106)

#資料清洗和字串轉數字
overseasStudentSchool$非學位生_大陸研修生<-gsub("…","NA",overseasStudentSchool$非學位生_大陸研修生)
overseasStudentSchool$非學位生_大陸研修生<-as.numeric(overseasStudentSchool$非學位生_大陸研修生)

ToTWNUniversity<-overseasStudentSchool%>%
  mutate(總數=rowSums(.[4:12],na.rm=T))%>%
  group_by(學校名稱)%>%
  summarise(總人數=sum(總數))%>%
  arrange(desc(總人數))

#第二題
ToTWNCountry_20Row<-rbind(top_n(ToTWNCountry,19),
                          slice(ToTWNCountry,20:n())%>%
                            summarise(國別="其他",總人數=sum(總人數)))

ToTWNCountryBar<-ToTWNCountry_20Row%>%
  ggplot(aes(x=reorder(國別,-總人數),y=總人數))+
  geom_bar(stat="identity")+
  theme_bw()+
  theme(axis.text.x = element_text(angle = 90, hjust = 1,vjust=0.4))+
  labs(x="國別")
ToTWNCountryBar

#第三題
#讀取shapefile
worldMap<-readShapeSpatial("ne_110m_admin_0_countries/ne_110m_admin_0_countries.shp")
#shapefile轉為data.frame
worldMap.df<-fortify(worldMap)
worldMap.df$id<-as.numeric(worldMap.df$id)
#建立表格
mydata<-data.frame(Name=worldMap$NAME_LONG,ISO3=worldMap$ISO_A3,id=seq(0,length(worldMap$ISO_A3)-1))
#因子轉字串和補遺漏值
mydata$ISO3<-as.character(mydata$ISO3)
mydata$ISO3[56]<-"FRA"
mydata$ISO3[119]<-"NOR"
worldMap.df<-left_join(worldMap.df,mydata,by="id")
#讀取國家中英對照表
countryName<-fromJSON("countries.json")
#國家中英對照表處理
index<-c(2,13,17,40,48,55,73,98,119,120,122,143,153,160,166,177,191,195,199,204,207,209,227,228,229,238)
Name<-c("阿拉伯聯合大公國","澳大利亞","波士尼亞與赫塞哥維納","剛果民主共和國","中國大陸","賽普勒斯","密克羅尼西亞",
        "克羅埃西亞","葛摩聯盟","聖克里斯多福","南韓","馬紹爾群島共和國","馬爾他","納米比亞","納戈爾諾-卡拉巴赫",
        "巴布亞紐幾內亞","塞爾維亞共和國","索羅門群島","新加坡","獅子山共和國","索馬利亞民主共和國","南蘇丹共和國",
        "千里達","吐瓦魯","臺灣","聖文森")
countryName$Taiwan[index]<-Name
#表格合併
worldMap.df<-left_join(worldMap.df,countryName,by="ISO3")
worldMap.df<-worldMap.df%>%
  select(long:ISO3,Taiwan)
colnames(worldMap.df)[10]<-"國別" 
final.data<-left_join(worldMap.df,ToTWNCountry,by="國別")

ToTWNCountryMap<-ggplot()+
  geom_polygon(data=final.data,aes(x=long,y=lat,group=group,fill=總人數),color="black",size=0.25)+
  coord_quickmap()+
  scale_fill_gradientn(colours=brewer.pal(7,"Blues"))+
  theme_void()
ToTWNCountryMap

#第四題_前
#讀取資料
#大專校院本國學生出國進修交流數
localStudent<-read_csv("Student_RPT.csv",skip=2)
#欄位名稱處理
ColName<-c("學年度","學期","學校設立別","學校類別","學校代碼","學校名稱","系所代碼","系所名稱","學制","對方學校國別",
          "對方學校中文名稱","對方學校英文名稱","本國學生出國進修、交流人數小計","本國學生出國進修、交流人數_男",
          "本國學生出國進修、交流人數_女")
colnames(localStudent)<-ColName
#刪除統計說明
localStudent<-localStudent[1:35020,]

FromTWNCountry<-localStudent%>%
  filter(學年度>=103)%>%
  group_by(對方學校國別)%>%
  summarise(人數=sum(`本國學生出國進修、交流人數小計`))%>%
  arrange(desc(人數))
head(FromTWNCountry,10)

#第四題_後
FromTWNUniversity<-localStudent%>%
  filter(學年度>=103)%>%
  group_by(學校名稱)%>%
  summarise(人數=sum(`本國學生出國進修、交流人數小計`))%>%
  arrange(desc(人數))
head(FromTWNUniversity,10)

#第五題
FromTWNUniversity_20Row<-rbind(top_n(FromTWNUniversity,19),
                               slice(FromTWNUniversity,20:n())%>%
                                 summarise(學校名稱="其他",人數=sum(人數)))
FromTWNCountryBar<-FromTWNUniversity_20Row%>%
  ggplot(aes(x=reorder(學校名稱,-人數),y=人數))+
  geom_bar(stat="identity")+
  theme_bw()+
  theme(axis.text.x = element_text(angle = 90, hjust = 1,vjust=0.4))+
  labs(x="學校名稱")

FromTWNCountryBar

#第六題
#讀取shapefile
worldMap<-readShapeSpatial("ne_110m_admin_0_countries/ne_110m_admin_0_countries.shp")

#shapefile轉為data.frame
worldMap.df<-fortify(worldMap)

#字串轉數字
worldMap.df$id<-as.numeric(worldMap.df$id)

#建立表格，包含地區名稱、地區ISO3碼、地區id
mydata<-data.frame(Name=worldMap$NAME_LONG,ISO3=worldMap$ISO_A3,id=seq(0,length(worldMap$ISO_A3)-1))

#因子轉字串和補遺漏值
mydata$ISO3<-as.character(mydata$ISO3)
mydata$ISO3[56]<-"FRA"
mydata$ISO3[119]<-"NOR"

#地圖資料合併表格(以id為依據，新增地圖名稱欄位、地區ISO3碼欄位)
worldMap.df<-left_join(worldMap.df,mydata,by="id")

#讀取國家中英對照表
countryName<-fromJSON("countries.json")

#國家中英對照表處理(對照表和開放資料的中文地區名稱不一致，以開放資料的地區名稱為依據，修改對照表的地區名稱)
index<-c(2,13,17,40,48,55,73,98,119,120,122,143,153,160,166,177,191,195,199,204,207,209,227,228,229,238)
Name<-c("阿拉伯聯合大公國","澳大利亞","波士尼亞與赫塞哥維納","剛果民主共和國","中國大陸","賽普勒斯","密克羅尼西亞",
        "克羅埃西亞","葛摩聯盟","聖克里斯多福","南韓","馬紹爾群島共和國","馬爾他","納米比亞","納戈爾諾-卡拉巴赫",
        "巴布亞紐幾內亞","塞爾維亞共和國","索羅門群島","新加坡","獅子山共和國","索馬利亞民主共和國","南蘇丹共和國",
        "千里達","吐瓦魯","臺灣","聖文森")
countryName$Taiwan[index]<-Name

#地區資料合併國家中英對照表(以ISO3碼為依據，主要目的為新增中文地區名稱欄位)
worldMap.df<-left_join(worldMap.df,countryName,by="ISO3")

#地區資料選取會用到的欄位
worldMap.df<-worldMap.df%>%
  select(long:ISO3,Taiwan)

#欄位名稱處理
colnames(worldMap.df)[10]<-"國別" 

#地區資料合併開放資料為最終資料(以國別為依據，新增總人數欄位)
final.data<-left_join(worldMap.df,ToTWNCountry,by="國別")

ToTWNCountryMap<-ggplot()+
  geom_polygon(data=final.data,aes(x=long,y=lat,group=group,fill=總人數),color="black",size=0.25)+
  coord_quickmap()+
  scale_fill_gradientn(colours=brewer.pal(7,"Blues"))+
  theme_void()

#顯示結果，灰色區域為無資料
ToTWNCountryMap