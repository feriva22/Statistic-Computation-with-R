#===================================== Persiapan Data dan Memfaktorkan Data ================================#
#load library for multinom
library(nnet)
library(caret)
mdata = read.csv(file.choose(), header = TRUE, sep = ",")

#variable Y
#kateg : a. stadium 0   c. stadium 2  e. stadium 4
#        b. stadium 1   d. stadium 3  f. stadium 5
mdata$kategoriStadium = factor(mdata$kategoriStadium)

#variable X1
#kateg : 1. 0-12 thun
#        2. 12-25 thun
#        3. >25 thun
mdata$usiaPenderita = factor(mdata$usiaPenderita,ordered = TRUE)


#variable X2 
#kateg : 1. laki-laki
#        2. perempuan
mdata$jenisKelamin = factor(mdata$jenisKelamin)

#variable X3
#kateg : 1. kelas A
#        2. kelas B
#        3. kelas I
#        4. kelas II
#        5. kelas III
mdata$kelasPerawatan = factor(mdata$kelasPerawatan, ordered = TRUE,levels = c('5','4','3','2','1'))

#variable X4
#kateg : 1. berpendidikan
#        2. tidak berpendidikan
mdata$pendidikan = factor(mdata$pendidikan, ordered = TRUE, levels = c('2','1'))
#variable X5
#kateg : 1. hari pertama
#        2. hari terakhir pengamatan
mdata$waktuPerawatan = factor(mdata$waktuPerawatan)

#===========================================================================================================#

#==================================== Mencari nilai Z values dan P value untuk ==============================#
#                             Menentukan variable respon yang signifikan (p value< 0.05)                     #

#mendapatkan nilai zvalue untuk mencari nilai variable yang signifikan , dengan dibandingkan nilai alpha (0.05)
getSignificanValue = function(themodel) {
  zvalues = summary(themodel)$coefficients / summary(themodel)$standard.errors
  hasil_pvalue = pnorm(abs(zvalues), lower.tail = FALSE)*2 #mendapatkan p_value
  label_attribut = attributes(hasil_pvalue[])$dimnames[[2]] #mendapatkan nilai attribut label
  total_dataYCategory = as.numeric(attributes(hasil_pvalue[])$dimnames[[1]]) #mendapatkan nilai atribut banyak kategori Y
  ujiHipotesis = matrix(list(), nrow =length(total_dataYCategory), ncol=length(label_attribut))
  colnames(ujiHipotesis) = label_attribut
  rownames(ujiHipotesis) = total_dataYCategory
  
  for (i in 2:length(label_attribut)){  #perualngan untuk membandingkan nilai apakah signifikan (pvalue < 0.05)
    for (j in total_dataYCategory){
      if(hasil_pvalue[j,i] < 0.05){
        ujiHipotesis[j,i] = 1
      }
      else{
        ujiHipotesis[j,i] = 0
      }
    }
  }
  #hasil_pvalue
  ujiHipotesis 
}

#============================================================================================================#


#==================================== Mencari nilai odd ratio untuk mengetahui seberapa======================#
#                                    besar variable Prediktor mempengaruhi variable Respon                   #

#odd ratio untuk menentukan seberapa besar pengaruhnya nilai odd ratio terhadap kategori variable Y
# misal nilai odd ration 2.4 maka mempengaruhi kategori stadium 2
getOddRatio = function(themodel){
  oddRatio = exp(coef(themodel))  #odd ratio 
  oddRatio
}

#============================================================================================================#

#==================================== Memprediksi nilai kebenaran dengan nilai Prediksi =====================#

#predict class dari model

getPredictModel = function(themodel, thedata){
  
  hasilPredictClass = predict(themodel, thedata)

  #menentukan tingkat kebenaran prediksi terhadap data asli, jika sama maka prediksi benar
  kebenaranPrediksi = rep(0, length(thedata$kategoriStadium)) #inisialisasi 50 banyak data, 0 untuk salah 1 untuk benar
  for(i in 1:length(thedata$kategoriStadium)){ #perulangan untuk mengecek kesamaan data prediksi dengan aktual
    if (thedata$kategoriStadium[i] == hasilPredictClass[i]){
      kebenaranPrediksi[i] = 1
    }else{
      kebenaranPrediksi[i] = 0
    }
  }
  dataHasilPrediksi = data.frame(hasilPredictClass,thedata$kategoriStadium,kebenaranPrediksi) #view data in data frame
  names(dataHasilPrediksi) = c('data Prediksi','data Aktual','nilai kebenaran')
  View(dataHasilPrediksi)
  table(hasilPredictClass, thedata$kategoriStadium)   #tabel confusion matrix
  
  #persentase kecocokan data dari seluruh data
  persentaseKecocokan = length(grep(1,kebenaranPrediksi))/length(hasilPredictClass)*100;
  paste("Persentase keakuratan data Prediksi dengan data aktual adalah",persentaseKecocokan,"%");
  
  return(hasilPredictClass)
}
#=============================================================================================================#

#=============================Memprediksi nilai kebenaran dengan menggunakan library Caret====================#
#table kebenaran 
#dari tiap baris akan ditentukan nilai kebenaran untuk tiap stadium dari seluruh
#responden di tiap baris, ini adalah hasil real dari uji
getPredictWithCaret = function(predictClass, thedata){
  library(caret)
  return(confusionMatrix( predictClass, thedata$kategoriStadium))
}

#=============================================================================================================#



#=================================== Membuat model dari seluruh variable Prediktor =========================#
#run the model
model = multinom(kategoriStadium ~ ., data = mdata)
#hasil coefisien dari model berguna untuk mendapatkan nilai odd ratio nanti
summary(model)

#get signifikan value
getSignificanValue(model)

#dari hasil diatas disimpulkan bahwa variable Prediktor KelasPerawatan memiliki nilai signifikan

#mencari oddRatio
getOddRatio(model)

#mencari nilai prediksi menggunakan predict
hasilPredict = getPredictModel(model,mdata)
#mencari nilai prediksi menggunakan caret 
caretPredict = getPredictWithCaret(hasilPredict, mdata)
caretPredict

#============================================================================================================#



#============================ Mencari model dengan menggunakan variable Prediktor=============================#
#                  KelasPerawatan karena memiliki nilai Signifikan pada perhitungan sebelumnya                #

modelWithSignifikan = multinom(kategoriStadium ~ kelasPerawatan, data = mdata)
summary(modelWithSignifikan)

#get signifikan value
getSignificanValue(modelWithSignifikan)
#dari hasil diatas disimpulkan bahwa variable Prediktor KelasPerawatan memiliki nilai signifikan

#mencari oddRatio
getOddRatio(modelWithSignifikan)

#mencari nilai prediksi menggunakan predict
hasilPredict2 = getPredictModel(modelWithSignifikan,mdata)
#mencari nilai prediksi menggunakan caret 
caretPredict2 = getPredictWithCaret(hasilPredict2, mdata)
caretPredict2
#============================================================================================================#

#============================================ Perbedaan nilai AIC tiap model=================================#

paste("Data nilai AIC model dengan semua variable Prediktor",model$AIC)
paste("Data nilai AIC modengan dengan variable Prediktor Kelas Perawatan",modelWithSignifikan$AIC)
if(model$AIC<modelWithSignifikan$AIC){
  print("Kesimpulan : model dengan semua variable Prediktor yang terbaik")
}else{
  print("Kesimpulan : model dengan variable Prediktor Kelas Perawatan yang terbaik")
}

#============================================================================================================#
#============================================ Perbedaan akurasi tiap model ==================================#

paste("Data nilai Akurasi model dengan semua variable Prediktor",caretPredict$overall[1]*100,"%")
paste("Data nilai Akurasi model dengan variable Prediktor Kelas Perawatan",caretPredict2$overall[1]*100,"%")
if(caretPredict$overall[1] > caretPredict2$overall[1]){
  print("Kesimpulan : model dengan semua variable Prediktor yang terbaik")
}else{
  print("Kesimpulan : model dengan variable Prediktor Kelas Perawatan yang terbaik")
}
