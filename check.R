library(knitr)
library(readxl)

# 
get_concentration = function(po2 = 55.0, 
                             pco2 = 24.6, 
                             ph = 7.19, 
                             hb = 20.5,
                             hct = 61.4, 	
                             temp = 37.0, 
                             p50 = 24.6){
  
  co2con = function(phe, satn, temp, hcrit, pco2){
    
    p = 7.4 - phe
    pk = 6.086 + 0.042 * p + (38.0 - temp) * (0.00472 + 0.00139 * p)
    sol = 0.0307 + 0.00057 * (37.0 - temp) + 0.00002 * (37.0 - temp) * (37.0 - temp)
    dox = 0.59 + 0.2913 * p-0.0844 * p^2
    dr = 0.664 + 0.2275 * p - 0.0938 * p^2
    ddd = dox + (dr-dox) * (1 - satn/100.0)
    cp = sol * pco2 * (1.0 + 10.0^(phe - pk))
    ccc = ddd * cp
    co2con = (hcrit * ccc * 0.01 + (1.0 - hcrit * 0.01) * cp) * 2.22
    return(co2con)
    
  }
  buflin = function(hb, po2, pco2, phe, p50, apco2 = 30, bpco2 = 60){
    
    satura = function(po2, pco2, phe, p50){
      
      a1 = -8532.229
      a2 = 2121.401
      a3 = -67.07399
      a4 = 935960.9
      a5 = -31346.26
      a6 = 2396.167
      a7 = -67.10441
      b = 0.43429 * log(40.0/pco2)
      x = po2 * 10.0^(0.024 * (37.0 - temp) + 0.4 * (phe - 7.4) + 0.06 * b)
      x = 26.8 * x / p50
      if(x <= 10){
        sat = 0.003683 * x + 0.000584 * x^2
      }else{
        sat=(x*(x*(x*(x+a3)+a2)+a1))/(x*(x*(x*(x+a7)+a6)+a5)+a4)
      }
      satura = 100.0 * sat
      return(satura)
      
    }
    
    y1 = 0.003 * hb * (100.0 - satura(po2 = po2, pco2 = pco2, phe = phe, p50 = p50))/100.0
    phx = 7.59 + y1 - 0.2741 * log(pco2/20.0)                              
    delph = phe - phx
    aph = 7.59+delph-0.2741 * log(apco2/20.0)
    bph = 7.59 + delph - 0.2741 * log(bpco2/20.0)
    aph_bph = c(aph, bph)
    names(aph_bph) = c("aph", "bph")
    return(aph_bph)
    
  }
  satura = function(po2, pco2, phe, p50){
    
    a1 = -8532.229
    a2 = 2121.401
    a3 = -67.07399
    a4 = 935960.9
    a5 = -31346.26
    a6 = 2396.167
    a7 = -67.10441
    b = 0.43429 * log(40.0/pco2)
    x = po2 * 10.0^(0.024 * (37.0 - temp) + 0.4 * (phe - 7.4) + 0.06 * b)
    x = 26.8 * x / p50
    if(x <= 10){
      sat = 0.003683 * x + 0.000584 * x^2
    }else{
      sat=(x*(x*(x*(x+a3)+a2)+a1))/(x*(x*(x*(x+a7)+a6)+a5)+a4)
    }
    satura = 100.0 * sat
    return(satura)
    
  }
  get_ph = function(pco2, y, aph, bph, apco2 = 30, bpco2 = 60){
    
    if(pco2 < 0.001){
      pco2 = 0.001
    }
    if(aph < 1){
      ph = 7.59 + y - 0.2741 * log(pco2/20.0)
    }else{
      ph = bph + y + (aph - bph) * log(pco2/bpco2)/log(apco2/bpco2)
    }
    return(ph)
    
  }
  
  aph = buflin(hb = hb, po2 = po2, pco2 = pco2, p50 = p50, phe = ph)[1]
  bph = buflin(hb = hb, po2 = po2, pco2 = pco2, p50 = p50, phe = ph)[2]
  # aph = 7.13
  # bph = 6.94
  
  ph1 = get_ph(pco2 = pco2, y = 0, aph = aph, bph = bph)
  y = 0.003 * hb * (1 - satura(po2 = po2, pco2 = pco2, phe = ph1, p50 = p50)/100.0)
  ph2 = get_ph(pco2, y = y, aph = aph, bph = bph)
  satrn = satura(po2 = po2, pco2 = pco2, phe = ph2, p50 = p50)
  names(satrn) = NULL
  co2_concentration = co2con(phe = ph2, satn = satrn, temp = temp, hcrit = hct, pco2 = pco2)
  o2_concentration = 0.0139 * hb * satrn + 0.003 * po2
  concentration = c(satrn, o2_concentration, co2_concentration, aph, bph)
  names(concentration) = c("O2 Saturation", "O2 Concentration", "CO2 Concentration", "APH", "BPH")
  return(concentration)
  
}

get_directory = function(){
  args <- commandArgs(trailingOnly = FALSE)
  file <- "--file="
  rstudio <- "RStudio"
  
  match <- grep(rstudio, args)
  if(length(match) > 0){
    return(dirname(rstudioapi::getSourceEditorContext()$path))
  }else{
    match <- grep(file, args)
    if (length(match) > 0) {
      return(dirname(normalizePath(sub(file, "", args[match]))))
    }else{
      return(dirname(normalizePath(sys.frames()[[1]]$ofile)))
    }
  }
}
wd = get_directory()
setwd(wd)

data = read_excel(path = "052319_Tibetan_Blood_Gas_2200.xlsx")

# kable(head(data))

data = data[data$EXERCISE == "M" & data$V_OR_A == "A",]

# kable(data)

result = vector()

for(i in 1:dim(data)[1]){
  result = rbind(result,get_concentration(po2 = data$PO2[i],
                    pco2 = data$PCO2[i],
                    ph = data$PH[i],
                    hb = data$HB[i],
                    hct = data$HCT[i],
                    temp = data$TEMP[i],
                    p50 = data$P50[i]))
}

kable(result)

