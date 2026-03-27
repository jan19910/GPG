### Sim für Sim_App_10_03.R

library(dplyr)
library(ggplot2)
library(gamlss)
library(profvis)

# General Structure of Simulation -----------------------------------------

make_structure_params <- function(no_researchers = 314,
                                  seed = 5597,
                                  year_start = 1900,
                                  act_year = 2025) {
  list(
    no_researchers = no_researchers,
    seed           = seed,
    year_start     = year_start,
    act_year       = act_year
  )
}

make_employee_params <- function(mu_entry        = 44,
                                 sigma_entry     = 3,
                                 max_age         = 69,
                                 lambda          = 4,
                                 early_exit_prob = 0.02,
                                 max_children    = 6,
                                 mu_tpf          = 500,
                                 sigma_tpf       = 2,
                                 p0_kinder_mann     = 0.40,
                                 p0_kinder_frau     = 0.40,
                                 lambda_kinder_mann = 1.5,
                                 lambda_kinder_frau = 1.5,
                                 p_german_mann = 0.90,
                                 p_german_frau = 0.89,
                                 p_eu          = 0.60) {
  list(
    mu_entry        = mu_entry,
    sigma_entry     = sigma_entry,
    max_age         = max_age,
    lambda          = lambda,
    early_exit_prob = early_exit_prob,
    max_children    = max_children,
    mu_tpf          = mu_tpf,
    sigma_tpf       = sigma_tpf,
    p0_kinder_mann     = p0_kinder_mann,
    p0_kinder_frau     = p0_kinder_frau,
    lambda_kinder_mann = lambda_kinder_mann,
    lambda_kinder_frau = lambda_kinder_frau,
    p_german_mann = p_german_mann,
    p_german_frau = p_german_frau,
    p_eu          = p_eu
  )
}

make_salary_params <- function(prob_W3  = 0.5,
                               p_lfunk       = 0.2,
                               p_lfunk_verlust = 0.1,
                               p_bb_start          = 0.3,
                               p_bb_gesamt         = 0.03,
                               p_bb_unbefr_anteil  = 0.4,
                               lambda_bb           = 500,
                               bb_befr_dauer       = 5,
                               min_jahre_neu       = 3,
                               gender_factor_bb_start  = 1.0,
                               gender_factor_bb_gesamt = 1.0,
                               gender_factor_bb_unbefr = 1.0,
                               gender_factor_bb_lambda = 1.0,
                               p_lbe_new        = 0.6,
                               p_lbe_verlaenger = 0.8,
                               p_lbe_entfrist   = 0.4) {
  list(
    prob_W3  = prob_W3,
    p_lfunk         = p_lfunk,
    p_lfunk_verlust = p_lfunk_verlust,
    p_bb_start          = p_bb_start,
    p_bb_gesamt         = p_bb_gesamt,
    p_bb_unbefr_anteil  = p_bb_unbefr_anteil,
    lambda_bb           = lambda_bb,
    bb_befr_dauer       = bb_befr_dauer,
    min_jahre_neu       = min_jahre_neu,
    gender_factor_bb_start  = gender_factor_bb_start,
    gender_factor_bb_gesamt = gender_factor_bb_gesamt,
    gender_factor_bb_unbefr = gender_factor_bb_unbefr,
    gender_factor_bb_lambda = gender_factor_bb_lambda,
    p_lbe_new        = p_lbe_new,
    p_lbe_verlaenger = p_lbe_verlaenger,
    p_lbe_entfrist   = p_lbe_entfrist
  )
}

make_gender_params <- function(prob_female_W2     = 0.4,
                               prob_female_W3     = 0.3,
                               gender_factor_new  = 1.0,
                               gender_factor_verl = 1.0,
                               gender_factor_entf = 1.0,
                               gender_factor_lfunk         = 1.0,
                               gender_factor_lfunk_verlust = 1.0,
                               gender_factor_tpf = 1.0) {
  list(
    prob_male_W2       = 1 - prob_female_W2,
    prob_male_W3       = 1 - prob_female_W3,
    gender_factor_new  = gender_factor_new,
    gender_factor_verl = gender_factor_verl,
    gender_factor_entf = gender_factor_entf,
    gender_factor_lfunk         = gender_factor_lfunk,
    gender_factor_lfunk_verlust = gender_factor_lfunk_verlust,
    gender_factor_tpf = gender_factor_tpf
  )
}


# Lbe_Stufen --------------------------------------------------------------

update_lbe_status <- function(active, p_new, p_verlaenger, p_entfrist, p_aufstieg,
                              gender_factor_new  = 1.0,
                              gender_factor_verl = 1.0,
                              gender_factor_entf = 1.0) {
  
  n <- nrow(active)
  if (n == 0) return(active)
  
  # Gender-adjustierte Wahrscheinlichkeiten
  is_female  <- active$Geschlecht == 2
  p_new_vec  <- ifelse(is_female, p_new        * gender_factor_new,  p_new)
  p_verl_vec <- ifelse(is_female, p_verlaenger * gender_factor_verl, p_verlaenger)
  p_entf_vec <- ifelse(is_female, p_entfrist   * gender_factor_entf, p_entfrist)
  
  # Zufallszahlen für alle Personen auf einmal
  rnd_verl <- runif(n)
  rnd_entf <- runif(n)
  rnd_new  <- runif(n)
  
  # Vektorisierte Update-Logik für eine Stufe
  update_stufe_vec <- function(status, jahre) {
    
    jahre_neu  <- jahre + as.integer(status > 0)
    status_neu <- status
    
    # Verlust: status==1, gerade Jahreszahl, Zufallszahl >= p_verl
    verlust <- status == 1 & jahre_neu %% 2 == 0 & rnd_verl >= p_verl_vec
    status_neu[verlust] <- 0L
    jahre_neu[verlust]  <- 0L
    
    # Entfristung: status==1 (nicht verloren), >= 6 Jahre, Zufallszahl < p_entf
    entfrist <- status_neu == 1 & jahre_neu >= 6 & rnd_entf < p_entf_vec
    status_neu[entfrist] <- 2L
    
    list(status = status_neu, jahre = jahre_neu)
  }
  
  # Alle 4 Stufen updaten
  s1 <- update_stufe_vec(active$lbe_s1_status, active$lbe_s1_jahre)
  active$lbe_s1_status <- s1$status
  active$lbe_s1_jahre  <- s1$jahre
  
  s2 <- update_stufe_vec(active$lbe_s2_status, active$lbe_s2_jahre)
  active$lbe_s2_status <- s2$status
  active$lbe_s2_jahre  <- s2$jahre
  
  s3 <- update_stufe_vec(active$lbe_s3_status, active$lbe_s3_jahre)
  active$lbe_s3_status <- s3$status
  active$lbe_s3_jahre  <- s3$jahre
  
  s4 <- update_stufe_vec(active$lbe_s4_status, active$lbe_s4_jahre)
  active$lbe_s4_status <- s4$status
  active$lbe_s4_jahre  <- s4$jahre
  
  # Neue Stufe vergeben
  stufen_matrix <- cbind(
    active$lbe_s1_status,
    active$lbe_s2_status,
    active$lbe_s3_status,
    active$lbe_s4_status
  )
  
  erste_freie <- max.col(stufen_matrix == 0, ties.method = "first")
  hat_freie   <- rowSums(stufen_matrix == 0) > 0
  
  darf_neu <- hat_freie &
    active$experience_this_year %% 2 == 0 &
    rnd_new < p_new_vec
  
  idx_neu <- which(darf_neu)
  for (i in idx_neu) {
    sf <- erste_freie[i]
    if (sf == 1) { active$lbe_s1_status[i] <- 1L; active$lbe_s1_jahre[i] <- 1L }
    if (sf == 2) { active$lbe_s2_status[i] <- 1L; active$lbe_s2_jahre[i] <- 1L }
    if (sf == 3) { active$lbe_s3_status[i] <- 1L; active$lbe_s3_jahre[i] <- 1L }
    if (sf == 4) { active$lbe_s4_status[i] <- 1L; active$lbe_s4_jahre[i] <- 1L }
  }
  
  return(active)
}


# update_lbe_status (loop) ------------------------------------------------



#update_lbe_status <- function(active, p_new, p_verlaenger, p_entfrist, p_aufstieg, 
#                              gender_factor_new  = 1.0,
#                              gender_factor_verl = 1.0,
#                              gender_factor_entf = 1.0) {
#  
#  n <- nrow(active)
#  
#    
#    update_stufe <- function(status, jahre, p_verlaenger, p_entfrist) {
#      
#      if (status == 0) {
#        return(list(status=0, jahre=0))  # neue Stufen werden unten bei naechste_freie vergeben
#        
#      } else if (status == 1) {
#        jahre <- jahre + 1
#        if (jahre %% 2 == 0) {
#          if (runif(1) >= p_verlaenger) return(list(status=0, jahre=0))
#        }
#        if (jahre >= 6 && runif(1) < p_entfrist) {
#          return(list(status=2, jahre=jahre))
#        }
#        
#      } 
#      
#      return(list(status=status, jahre=jahre))
#    }
#    
#    for (i in 1:n) {
#      
#      if (active$Geschlecht[i] == 2) {
#        p_new_i        <- p_new        * gender_factor_new
#        p_verlaenger_i <- p_verlaenger * gender_factor_verl
#        p_entfrist_i   <- p_entfrist   * gender_factor_entf
#      } else {
#        p_new_i        <- p_new
#        p_verlaenger_i <- p_verlaenger
#        p_entfrist_i   <- p_entfrist
#      }
#      
#      # Alle vier Stufen unabhängig updaten
#      s1 <- update_stufe(active$lbe_s1_status[i], active$lbe_s1_jahre[i], p_verlaenger_i, p_entfrist_i)
#      active$lbe_s1_status[i] <- s1$status
#      active$lbe_s1_jahre[i]  <- s1$jahre
#      
#      s2 <- update_stufe(active$lbe_s2_status[i], active$lbe_s2_jahre[i], p_verlaenger_i, p_entfrist_i)
#      active$lbe_s2_status[i] <- s2$status
#      active$lbe_s2_jahre[i]  <- s2$jahre
#      
#      s3 <- update_stufe(active$lbe_s3_status[i], active$lbe_s3_jahre[i], p_verlaenger_i, p_entfrist_i)
#      active$lbe_s3_status[i] <- s3$status
#      active$lbe_s3_jahre[i]  <- s3$jahre
#      
#      s4 <- update_stufe(active$lbe_s4_status[i], active$lbe_s4_jahre[i], p_verlaenger_i, p_entfrist_i)
#      active$lbe_s4_status[i] <- s4$status
#      active$lbe_s4_jahre[i]  <- s4$jahre
#      
#      # Danach: nur eine neue Stufe einführen
#      stufen <- c(active$lbe_s1_status[i], active$lbe_s2_status[i],
#                  active$lbe_s3_status[i], active$lbe_s4_status[i])
#      naechste_freie <- which(stufen == 0)[1]
#      
#      if (!is.na(naechste_freie) && active$experience_this_year[i] %% 2 == 0 && runif(1) < p_new_i) {
#        if (naechste_freie == 1) { active$lbe_s1_status[i] <- 1; active$lbe_s1_jahre[i] <- 1 }
#        if (naechste_freie == 2) { active$lbe_s2_status[i] <- 1; active$lbe_s2_jahre[i] <- 1 }
#        if (naechste_freie == 3) { active$lbe_s3_status[i] <- 1; active$lbe_s3_jahre[i] <- 1 }
#        if (naechste_freie == 4) { active$lbe_s4_status[i] <- 1; active$lbe_s4_jahre[i] <- 1 }
#      }
#    }
#  
#  
#  return(active)
#  
#}


# Funktionszuschläge ------------------------------------------------------


update_lfunk <- function(active, p_lfunk, p_lfunk_verlust,
                         gender_factor_lfunk        = 1.0,
                         gender_factor_lfunk_verlust = 1.0) {
  
  if (nrow(active) == 0) return(active)
  
  for (i in 1:nrow(active)) {
    
    if (active$Geschlecht[i] == 2) {
      p_lfunk_i        <- p_lfunk        * gender_factor_lfunk
      p_lfunk_verlust_i <- p_lfunk_verlust * gender_factor_lfunk_verlust
    } else {
      p_lfunk_i        <- p_lfunk
      p_lfunk_verlust_i <- p_lfunk_verlust
    }
    
    if (active$LFunk[i] == 0) {
      if (runif(1) < p_lfunk_i) {
        active$LFunk[i]       <- 1
        active$lfunk_jahre[i] <- 1
      }
    } else if (active$LFunk[i] == 1) {
      if (runif(1) < p_lfunk_verlust_i) {
        active$LFunk[i]       <- 0
        active$lfunk_jahre[i] <- 0
      } else {
        active$lfunk_jahre[i] <- active$lfunk_jahre[i] + 1
      }
    }
  }
  return(active)
}


# Berufungs-/Bleibezuschläge ----------------------------------------------


update_bb <- function(active, p_bb_gesamt, p_bb_unbefr_anteil,
                      lambda_bb, bb_befr_dauer, min_jahre_neu,
                      gender_factor_bb_start  = 1.0,
                      gender_factor_bb_gesamt = 1.0,
                      gender_factor_bb_unbefr = 1.0,
                      gender_factor_bb_lambda = 1.0) {
  
  if (nrow(active) == 0) return(active)
  
  for (i in 1:nrow(active)) {
    
    # Gender Faktoren anwenden
    is_female <- active$Geschlecht[i] == 2
    p_bb_gesamt_i        <- ifelse(is_female, p_bb_gesamt        * gender_factor_bb_gesamt, p_bb_gesamt)
    p_bb_unbefr_anteil_i <- ifelse(is_female, p_bb_unbefr_anteil * gender_factor_bb_unbefr, p_bb_unbefr_anteil)
    lambda_i             <- ifelse(is_female, lambda_bb           * gender_factor_bb_lambda, lambda_bb)
    
    # Befristeten Zuschlag updaten
    if (active$bb_befr_betrag[i] > 0) {
      active$bb_befr_jahre[i] <- active$bb_befr_jahre[i] + 1
      if (active$bb_befr_jahre[i] >= bb_befr_dauer) {
        active$bb_befr_betrag[i] <- 0
        active$bb_befr_jahre[i]  <- 0
      }
    }
    
    # Zähler hochzählen
    if (active$bb_letzte_vergabe[i] > 0) {
      active$bb_letzte_vergabe[i] <- active$bb_letzte_vergabe[i] + 1
    }
    
    kann_neue_vergabe <- active$bb_letzte_vergabe[i] == 0 ||
      active$bb_letzte_vergabe[i] > min_jahre_neu
    if (kann_neue_vergabe) {
      if (runif(1) < p_bb_gesamt_i) {
        active$bb_letzte_vergabe[i] <- 1
        if (runif(1) < p_bb_unbefr_anteil_i) {
          active$bb_unbefr_betrag[i] <- active$bb_unbefr_betrag[i] + rpois(1, lambda_i)
        } else {
          active$bb_befr_betrag[i] <- rpois(1, lambda_i)
          active$bb_befr_jahre[i]  <- 1
        }
      }
    } else if (active$bb_letzte_vergabe[i] > 0) {
      active$bb_letzte_vergabe[i] <- active$bb_letzte_vergabe[i] + 1
    }
  }
  return(active)
}

run_sim <- function(structure_params,
                    emp_params, 
                    salary_params, 
                    gender_params){

  # Parameter auspacken -----------------------------------------------------  
  
  no_researchers  <- structure_params$no_researchers
  seed            <- structure_params$seed
  year_start      <- structure_params$year_start
  act_year        <- structure_params$act_year
  mu_entry        <- emp_params$mu_entry
  sigma_entry     <- emp_params$sigma_entry
  max_age         <- emp_params$max_age
  lambda          <- emp_params$lambda
  early_exit_prob <- emp_params$early_exit_prob
  max_children    <- emp_params$max_children
  mu_tpf    <- emp_params$mu_tpf
  sigma_tpf <- emp_params$sigma_tpf
  p0_kinder_mann     <- emp_params$p0_kinder_mann
  p0_kinder_frau     <- emp_params$p0_kinder_frau
  lambda_kinder_mann <- emp_params$lambda_kinder_mann
  lambda_kinder_frau <- emp_params$lambda_kinder_frau
  p_german_mann <- emp_params$p_german_mann
  p_german_frau <- emp_params$p_german_frau
  p_eu          <- emp_params$p_eu
  
  prob_W3         <- salary_params$prob_W3
  p_lfunk         <- salary_params$p_lfunk
  p_lfunk_verlust <- salary_params$p_lfunk_verlust
  p_bb_start         <- salary_params$p_bb_start
  p_bb_gesamt        <- salary_params$p_bb_gesamt
  p_bb_unbefr_anteil <- salary_params$p_bb_unbefr_anteil
  lambda_bb          <- salary_params$lambda_bb
  bb_befr_dauer      <- salary_params$bb_befr_dauer
  min_jahre_neu      <- salary_params$min_jahre_neu
  gender_factor_bb_start  <- salary_params$gender_factor_bb_start
  gender_factor_bb_gesamt <- salary_params$gender_factor_bb_gesamt
  gender_factor_bb_unbefr <- salary_params$gender_factor_bb_unbefr
  gender_factor_bb_lambda <- salary_params$gender_factor_bb_lambda
  p_lbe_new        <- salary_params$p_lbe_new
  p_lbe_verlaenger <- salary_params$p_lbe_verlaenger
  p_lbe_entfrist   <- salary_params$p_lbe_entfrist
  
  prob_male_W2 <- gender_params$prob_male_W2
  prob_male_W3 <- gender_params$prob_male_W3
  gender_factor_new  <-  gender_params$gender_factor_new
  gender_factor_verl <-  gender_params$gender_factor_verl
  gender_factor_entf <-  gender_params$gender_factor_entf
  gender_factor_lfunk         <- gender_params$gender_factor_lfunk
  gender_factor_lfunk_verlust <- gender_params$gender_factor_lfunk_verlust
  gender_factor_tpf <- gender_params$gender_factor_tpf
  
  set.seed(seed)
  

# Parameter ---------------------------------------------------------------

  no_faculties = 14
  faculty_sizes = c(2, 2, 2, 2, 3, 3, 4, 4, 4, 5, 6, 7, 8, 9)
  no_researchers_per_faculty = data.frame(t(rmultinom(1, no_researchers, p=faculty_sizes)))
  sigma_entry <- 3
  lambda <- 4       
  

# Create initial employees: -----------------------------------------------

  create_new_employee = function(year, no_researchers_per_faculty){
    
    no_emp = sum(no_researchers_per_faculty)
    mock_data = list()
    mock_data$EmpNum <- seq(1, no_emp) + (year - year_start) * 1000
    mock_data$Vergütungsgruppe = sample(c("W2","W3"), size=no_emp, prob=c(0.5,0.5), replace=T)
    mock_data$Geschlecht <- NA
    idx_W2 <- mock_data$Vergütungsgruppe == "W2"
    idx_W3 <- mock_data$Vergütungsgruppe == "W3"
    
    mock_data$Geschlecht[idx_W2] <- sample(1:2, sum(idx_W2), 
                                           prob=c(prob_male_W2, 1-prob_male_W2), 
                                           replace=TRUE)
    mock_data$Geschlecht[idx_W3] <- sample(1:2, sum(idx_W3), 
                                           prob=c(prob_male_W3, 1-prob_male_W3), 
                                           replace=TRUE)
    mock_data$Berichtsjahr = year
    mock_data$Fachgebiet = rep(colnames(no_researchers_per_faculty), times=no_researchers_per_faculty)
    mock_data$entry_age <- entry_age <- round(rnorm(no_emp, mean = mu_entry, sd = sigma_entry))
    simulate_retirement_poisson <- function(entry_age, max_age, lambda, early_exit_prob) {
      if (entry_age >= max_age) return(max_age)
      repeat {
        x <- rpois(1, lambda)
        y <- max_age - x  
        if(runif(1) < early_exit_prob) {
          y <- entry_age + sample(0:(max_age - entry_age), 1)  
        }
        if(y >= entry_age && y <= max_age) return(y)
      }
    }
    
    mock_data$retirement_ages <- sapply(entry_age, simulate_retirement_poisson,
                                        max_age = max_age, lambda = lambda,
                                        early_exit_prob = early_exit_prob)
    mock_data$Erfahrung <- mock_data$retirement_ages - mock_data$entry_age
    mock_data$Geburtsjahr <- year - mock_data$entry_age
    
    #Vergütungsgruppe und Leistungsbezüge
    mock_data$lbe_s1_status <- 0
    mock_data$lbe_s1_jahre  <- 0
    mock_data$lbe_s2_status <- 0
    mock_data$lbe_s2_jahre  <- 0
    mock_data$lbe_s3_status <- 0
    mock_data$lbe_s3_jahre  <- 0
    mock_data$lbe_s4_status <- 0
    mock_data$lbe_s4_jahre  <- 0
    mock_data$lfunk_jahre <- 0
    mock_data$LFunk      <- 0
    mock_data$bb_befr_betrag    <- 0
    mock_data$bb_befr_jahre     <- 0
    mock_data$bb_unbefr_betrag  <- 0
    mock_data$bb_letzte_vergabe <- 0
    
  
    gender_factor <- ifelse(mock_data$Geschlecht == 2, gender_factor_tpf, 1)  
    mock_data$ThirdPartyFunding <- round(rGA(no_emp, mu = mu_tpf, sigma = sigma_tpf) * gender_factor)
    
    simulate_children <- function(n, p0, lambda) {
      zero <- rbinom(n, size = 1, prob = p0)
      kids <- ifelse(zero == 1, 0, rpois(n, lambda))
      pmin(kids, max_children)
    }
    mock_data$Kinder <- NA_integer_
    mock_data$Kinder[mock_data$Geschlecht == 1] <- simulate_children(
      n      = sum(mock_data$Geschlecht == 1),
      p0     = p0_kinder_mann,
      lambda = lambda_kinder_mann
    )
    mock_data$Kinder[mock_data$Geschlecht == 2] <- simulate_children(
      n      = sum(mock_data$Geschlecht == 2),
      p0     = p0_kinder_frau,
      lambda = lambda_kinder_frau
    )
    
    
    # Unter den Nicht-Deutschen: EU vs Others
    p_german  <- ifelse(mock_data$Geschlecht == 1, p_german_mann, p_german_frau)
    is_german <- rbinom(no_emp, size = 1, prob = p_german)
    nationality <- ifelse(is_german == 1, "German",
                          ifelse(runif(no_emp) < p_eu, "EU", "Others"))
    
    mock_data$Nationality <- factor(nationality, levels = c("German", "EU", "Others"))
    
    p_married <- ifelse(mock_data$Geschlecht == 1, 0.79, 0.57)
    is_married <- rbinom(no_emp, size = 1, prob = p_married)
    
    # Unter den Nicht-Verheirateten: Single vs Geschieden/Witwe
    p_single_among_notmarried <- 0.70
    marital <- ifelse(is_married == 1, "married",
                      ifelse(runif(no_emp) < p_single_among_notmarried, "single", "divorced_widowed"))
    
    mock_data$MaritalStatus <- factor(marital, levels = c("married", "single", "divorced_widowed"))
    
    mock_data = as.data.frame(mock_data)    
    
    # Startzuschlag vergeben
    hat_bb_start <- ifelse(
      mock_data$Geschlecht == 2,
      runif(no_emp) < p_bb_start * gender_factor_bb_start,
      runif(no_emp) < p_bb_start
    )
    idx_start <- which(hat_bb_start)
    for (i in idx_start) {
      mock_data$bb_letzte_vergabe[i] <- 1
      is_female_i  <- mock_data$Geschlecht[i] == 2
      p_unbefr_i   <- ifelse(is_female_i, p_bb_unbefr_anteil * gender_factor_bb_unbefr, p_bb_unbefr_anteil)
      lambda_i     <- ifelse(is_female_i, lambda_bb           * gender_factor_bb_lambda, lambda_bb)
      if (runif(1) < p_unbefr_i) {
        mock_data$bb_unbefr_betrag[i] <- rpois(1, lambda_i)
      } else {
        mock_data$bb_befr_betrag[i] <- rpois(1, lambda_i)
        mock_data$bb_befr_jahre[i]  <- 1
      }
    }
    


    return(mock_data)
  }
  
  mock_data = create_new_employee(year_start, no_researchers_per_faculty[1,])
 
  years <- year_start:act_year
  results_list <- vector("list", length(years))
  # Startbestand
  mock_data_current <- mock_data %>%
    distinct(EmpNum, .keep_all = TRUE)
  

# Yearly loop start -------------------------------------------------------
  
  
  for (year in years) {
    
    # Nur aktive Personen behalten
    active <- mock_data_current[
      (year - mock_data_current$Geburtsjahr) <= mock_data_current$retirement_ages,
      , drop = FALSE
    ]
    
    # Neueinstellungen 
    
    target_total <- 314
    n_current <- nrow(active)
    n_enter <- target_total - (n_current)
    n_enter <- max(n_enter, 0)
    
    
    if (n_enter > 0) {
      
      fac_levels <- colnames(no_researchers_per_faculty)
      
      sampled_fac <- sample(
        fac_levels,
        size = n_enter,
        replace = TRUE,
        prob = faculty_sizes
        #     prob = prob_fac
      )
      
      entering_counts <- as.integer(table(factor(sampled_fac, levels = fac_levels)))
      entering_df <- as.data.frame(t(entering_counts))
      colnames(entering_df) <- fac_levels
      
      new_employees <- create_new_employee(year, entering_df)
      active <- bind_rows(active, new_employees)%>%
        distinct(EmpNum, .keep_all = TRUE)
      
    }

    # Berichtsjahr setzen 
    if (nrow(active) > 0) {
      active$Berichtsjahr <- year
    }
    
    # Erfahrung im aktuellen Jahr berechnen
    active$experience_this_year <- year - active$Geburtsjahr - active$entry_age
    

# LBE_active --------------------------------------------------------------



    active <- update_lbe_status(                
      active,
      p_new        = p_lbe_new,
      p_verlaenger = p_lbe_verlaenger,
      p_entfrist   = p_lbe_entfrist,
      gender_factor_new  = gender_params$gender_factor_new,
      gender_factor_verl = gender_params$gender_factor_verl,
      gender_factor_entf = gender_params$gender_factor_entf
    )
    
    
    
    # Befristete Stufen zählen (status == 1)
    active$lbe_befr_gesamt <- (active$lbe_s1_status == 1) +
                              (active$lbe_s2_status == 1) +
                              (active$lbe_s3_status == 1) +
                              (active$lbe_s4_status == 1)
    
    # Entfristete Stufen zählen (status == 2)
    active$lbe_unbefr_gesamt <- (active$lbe_s1_status == 2) +
                                (active$lbe_s2_status == 2) +
                                (active$lbe_s3_status == 2) +
                                (active$lbe_s4_status == 2)
    
    active$lbe_befr_euro   <-   active$lbe_befr_gesamt   * 350
    active$lbe_unbefr_euro <-   active$lbe_unbefr_gesamt * 350
    active$lbe_gesamt_euro <-   active$lbe_befr_euro + active$lbe_unbefr_euro
    
    active <- update_lfunk(active, 
                           p_lfunk                     = p_lfunk, 
                           p_lfunk_verlust             = p_lfunk_verlust,
                           gender_factor_lfunk         = gender_factor_lfunk,
                           gender_factor_lfunk_verlust = gender_factor_lfunk_verlust)

    active$lfunk_euro <-  ifelse(active$lfunk_jahre == 0, 0,
                          ifelse(active$lfunk_jahre <= 2, 700,
                          ifelse(active$lfunk_jahre <= 5, 1050,
                                                          1400)))
    
    active <- update_bb(
      active,
      p_bb_gesamt        = p_bb_gesamt,
      p_bb_unbefr_anteil = p_bb_unbefr_anteil,
      lambda_bb          = lambda_bb,
      bb_befr_dauer      = bb_befr_dauer,
      min_jahre_neu      = min_jahre_neu,
      gender_factor_bb_start  = gender_factor_bb_start,
      gender_factor_bb_gesamt = gender_factor_bb_gesamt,
      gender_factor_bb_unbefr = gender_factor_bb_unbefr,
      gender_factor_bb_lambda = gender_factor_bb_lambda
    )

    results_list[[which(years == year)]] <- active
    
    # Übergabe an nächstes Jahr 
    mock_data_current <- active
  }
  
  mock_data_all <- bind_rows(results_list)
  
  # Group by ----------------------------------------------------------------
  
  mock_data_all <- mock_data_all %>%
    group_by(EmpNum) %>%
    arrange(Berichtsjahr) %>%
    mutate(
      # Kinder können sich über Zeit erhöhen (nicht verringern)
      Kinder_cumulative = cumsum(rbinom(n(), 1, prob = 0.03)),  # 3% Chance pro Jahr
      Kinder = pmin(first(Kinder) + Kinder_cumulative, max_children),

    
      # MaritalStatus kann sich ändern
      MaritalStatus = {
        status <- rep(first(MaritalStatus), n())
        if(n() > 1) {
          for(i in 2:n()) {
            if(runif(1) < 0.02) {  # 2% Chance pro Jahr
              if(status[i-1] == "single") status[i] <- "married"
              else if(status[i-1] == "married") status[i] <- "divorced_widowed"
              else status[i] <- status[i-1]
            } else {
              status[i] <- status[i-1]
            }
          }
        }
        factor(status, levels = c("married", "single", "divorced_widowed"))
      },
      
      # Aktuelle Berufserfahrung berechnen
      experience_current = Berichtsjahr - Geburtsjahr - first(entry_age),
      
      # ThirdPartyFunding variiert jährlich
      ThirdPartyFunding = pmax(0, round(first(ThirdPartyFunding) * rnorm(n(), mean = 1, sd = 0.15))),

      
    ) %>%
    ungroup() %>%
    select(-Kinder_cumulative, -experience_current)
 
  
  return(mock_data_all)
  
}

test <- run_sim(
  make_structure_params(),
  make_employee_params(),
  make_salary_params(),
  make_gender_params()
)

# Nur letztes Jahr anschauen
last_year <- test[test$Berichtsjahr == 2025, ]


# Ein paar Personen über Zeit anschauen
test[test$EmpNum == 1, c("Berichtsjahr", "lbe_s1_status", "lbe_s1_jahre", 
                         "lbe_s2_status", "lbe_befr_gesamt", "lbe_unbefr_gesamt")]



test1 <- run_sim(make_structure_params(seed = 111), make_employee_params(), make_salary_params(), make_gender_params())
test2 <- run_sim(make_structure_params(seed = 999), make_employee_params(), make_salary_params(), make_gender_params())

identical(test1$ThirdPartyFunding, test2$ThirdPartyFunding)

#profvis({
#  for (i in 1:5) {
#    run_sim(
#      make_structure_params(seed = i),
#      make_employee_params(),
#      make_salary_params(),
#      make_gender_params()
#    )
#  }
#})
#
#Rprof("profile.out", interval = 0.01)
#run_sim(
#  make_structure_params(),
#  make_employee_params(),
#  make_salary_params(),
#  make_gender_params()
#)
#Rprof(NULL)
#summaryRprof("profile.out")$by.self
#
#system.time({
#  run_sim(
#    make_structure_params(year_start = 2000, act_year = 2010),
#    make_employee_params(),
#    make_salary_params(),
#    make_gender_params()
#  )
#})
