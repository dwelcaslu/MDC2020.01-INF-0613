
prep_incomplete_data <- function(carros){

  print(colSums(carros == '?'))
  
  # normalized_losses
  carros$normalized_losses <- as.numeric(carros$normalized_losses)
  carros$normalized_losses[is.na(carros$normalized_losses)] = mean(carros$normalized_losses, na.rm=TRUE)
  # num_of_doors
  carros$num_of_doors[carros$num_of_doors=='?'] = 'four'
  # bore -- calibre
  carros$bore <- as.numeric(carros$bore)
  carros$bore[is.na(carros$bore)] = mean(carros$bore, na.rm=TRUE)
  # price
  carros$price <- as.numeric(carros$price)
  carros$price[is.na(carros$price)] = mean(carros$price, na.rm=TRUE)
  # stroke
  carros$stroke <- as.numeric(carros$stroke)
  carros$stroke[is.na(carros$stroke)] = mean(carros$stroke, na.rm=TRUE)
  # horsepower
  carros$horsepower <- as.numeric(carros$horsepower)
  carros$horsepower[is.na(carros$horsepower)] = mean(carros$horsepower, na.rm=TRUE)
  # peak_rpm
  carros$peak_rpm <- as.numeric(carros$peak_rpm)
  carros$peak_rpm[is.na(carros$peak_rpm)] = mean(carros$peak_rpm, na.rm=TRUE)

  return(carros)
}

encode_features <- function(carros){

  # One-hot encode
  # feature fuel_type
  carros$diesel <- as.numeric(carros$fuel_type == "diesel")
  carros$gas <- as.numeric(carros$fuel_type == "gas")
  carros$fuel_type <- NULL
  # feature aspiration
  carros$std <- as.numeric(carros$aspiration == "std")
  carros$turbo <- as.numeric(carros$aspiration == "turbo")
  carros$aspiration <- NULL
  # feature num_of_doors
  carros$four_doors <- as.numeric(carros$num_of_doors == "four")
  carros$two_doors <- as.numeric(carros$num_of_doors == "two")
  carros$num_of_doors <- NULL
  #feature body_style
  carros$convertible <- as.numeric(carros$body_style == "convertible")
  carros$hardtop <- as.numeric(carros$body_style == "hardtop")
  carros$hatchback <- as.numeric(carros$body_style == "hatchback")
  carros$sedan <- as.numeric(carros$body_style == "sedan")
  carros$wagon <- as.numeric(carros$body_style == "wagon")
  carros$body_style <- NULL
  #feature drive_wheels
  carros$wd4_wheels <- as.numeric(carros$drive_wheels == "4wd")
  carros$fwd_wheels <- as.numeric(carros$drive_wheels == "fwd")
  carros$rwd_wheels <- as.numeric(carros$drive_wheels == "rwd")
  carros$drive_wheels <- NULL
  # feature engine_location
  carros$engine_front <- as.numeric(carros$engine_location == "front")
  carros$engine_rear <- as.numeric(carros$engine_location == "rear")
  carros$engine_location <- NULL
  #feature engine_type
  carros$eng_type_dohc <- as.numeric(carros$engine_type == "dohc")
  carros$eng_type_dohcv <- as.numeric(carros$engine_type == "dohcv")
  carros$eng_type_l <- as.numeric(carros$engine_type == "l")
  carros$eng_type_ohc <- as.numeric(carros$engine_type == "ohc")
  carros$eng_type_ohcf <- as.numeric(carros$engine_type == "ohcf")
  carros$eng_type_ohcv <- as.numeric(carros$engine_type == "ohcv")
  carros$eng_type_rotor <- as.numeric(carros$engine_type == "rotor")
  carros$engine_type <- NULL
  #feature num_of_cylinders
  carros$eight_cyl <- as.numeric(carros$num_of_cylinders == "eight")
  carros$five_cyl <- as.numeric(carros$num_of_cylinders == "five")
  carros$four_cyl <- as.numeric(carros$num_of_cylinders == "four")
  carros$six_cyl <- as.numeric(carros$num_of_cylinders == "six")
  carros$three_cyl <- as.numeric(carros$num_of_cylinders == "three")
  carros$twelve_cyl <- as.numeric(carros$num_of_cylinders == "twelve")
  carros$two_cyl <- as.numeric(carros$num_of_cylinders == "two")
  carros$num_of_cylinders <- NULL
  #feature fuel_system
  carros$bbl1_fuel <- as.numeric(carros$fuel_system == "1bbl")
  carros$bbl2_fuel <- as.numeric(carros$fuel_system == "2bbl")
  carros$bbl4_fuel <- as.numeric(carros$fuel_system == "4bbl")
  carros$idi_fuel <- as.numeric(carros$fuel_system == "idi")
  carros$mfi_fuel <- as.numeric(carros$fuel_system == "mfi")
  carros$mpfi_fuel <- as.numeric(carros$fuel_system == "mpfi")
  carros$spdi_fuel <- as.numeric(carros$fuel_system == "spdi")
  carros$spfi_fuel <- as.numeric(carros$fuel_system == "spfi")
  carros$fuel_system <- NULL
 
  return(carros) 
}


min_max_normalize <- function(data, nonfeat_cols){

  max_val <- apply(data[, -nonfeat_cols], 2, max)
  min_val <- apply(data[, -nonfeat_cols], 2, min)
  min_max_diff <- (max_val - min_val)
  data[, -nonfeat_cols] <- sweep(data[, -nonfeat_cols], 2, min_val, "-")
  data[, -nonfeat_cols] <- sweep(data[, -nonfeat_cols], 2, min_max_diff, "/")
  
  return(data)
}
