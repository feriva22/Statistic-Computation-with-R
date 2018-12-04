#contoh soal hubungan jumlah sales dan jumlah penjualan
totSales = c(4,5,5,3,4,2,5,3,4,4,3,5,9,2,2,3,5,3,4,4,4,4,3,5,9,2,2,3,5,3)
totPenjualan = c(325,443,454,368,353,256,422,274,366,337,357,479,580,266,255,255,414,284,361,331,265,337,257,479,480,266,300,212,331,187)

#hipotesis
h0 = "tidak ada hubungan jumlah sales dan jumlah penjualan"
h1 = "ada hubungan jumlah sales dan julmah penjualan"

hasil = cor.test(totSales,totPenjualan, method = "pearson")

#uji hipotesis
if(hasil$p.value<0.05){
  keputusan = "tolak h0"
  kesimpulan = h1
}else{
  keputusan = "terima h0"
  kesimpulan = h0
}
keputusan
kesimpulan
