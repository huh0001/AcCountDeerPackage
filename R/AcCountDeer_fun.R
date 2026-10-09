
#' Run the AcCountDeer population model
#'
#' @export

AcCountDeer <- function(
    preharvest_pop = preharvest_pop,
    doe_ratio = doe_ratio,
    adult_prop = adult_prop,
    y1_prop = y1_prop,
    yearling_prop = yearling_prop,
    fawn_prop = fawn_prop,
    adult_fawn_rate = adult_fawn_rate,
    yearling_fawn_rate = yearling_fawn_rate,
    antlered_harvest = antlered_harvest,
    antlerless_harvest = antlerless_harvest,
    m_yearling_harvest_prop = m_yearling_harvest_prop,
    f_yearling_harvest_prop = f_yearling_harvest_prop,
    fawn_harvest_prop = fawn_harvest_prop,
    birth_month = birth_month,
    season_open = season_open,
    season_duration = season_duration,
    adult_male_nhm = adult_male_nhm,
    adult_female_nhm = adult_female_nhm,
    yearling_male_nhm = yearling_male_nhm,
    yearling_female_nhm = yearling_female_nhm,
    fawn_male_nhm = fawn_male_nhm,
    fawn_female_nhm = fawn_female_nhm,
    six_month_fawn_nhm = six_month_fawn_nhm,

    hc_adult_male = 0,
    hc_adult_female = 0,
    hc_y1_male = 0,
    hc_y1_female = 0,
    hc_yearling_male = 0,
    hc_yearling_female = 0,
    hc_fawn_male = 0,
    hc_fawn_female = 0,

    adm_adult_male = 0,
    adm_adult_female = 0,
    adm_y1_male = 0,
    adm_y1_female = 0,
    adm_yearling_male = 0,
    adm_yearling_female = 0,
    adm_fawn_male = 0,
    adm_fawn_female = 0,

    hc_duration = 0,
    adm_duration = 0,

    hc_specify = 0,
    adm_specify = 0,

    project_years = 15
) {

  ## Value Validation
  hc_validation <- c(
    hc_adult_male,
    hc_adult_female,
    hc_y1_male,
    hc_y1_female,
    hc_yearling_male,
    hc_yearling_female,
    hc_fawn_male,
    hc_fawn_female
  )

  if (any(is.na(hc_validation)) ||
      any(hc_validation < -1 | hc_validation > 1)) {
    stop("All hc values must be between -1 and 1 and cannot be NA.")
  }

  adm_validation <- c(
    adm_adult_male,
    adm_adult_female,
    adm_y1_male,
    adm_y1_female,
    adm_yearling_male,
    adm_yearling_female,
    adm_fawn_male,
    adm_fawn_female
  )

  if (any(adm_validation < 0 | adm_validation > 1)) {
    stop("All adm values must be between 0 and 1.")
  }

  # Check month/season inputs
  month_validation <- c(
    birth_month,
    season_open,
    season_duration
  )

  if (any(is.na(month_validation)) ||
      any(month_validation < 0 | month_validation > 12)) {
    stop("birth_month, season_open, and season_duration must be between 0 and 12.")
  }

  # Check population proportion inputs
  prop_validation <- c(
    adult_prop,
    y1_prop,
    yearling_prop,
    fawn_prop
  )

  if (any(is.na(prop_validation)) ||
      any(prop_validation < 0 | prop_validation > 1)) {
    stop("Proportions must be between 0 and 1.")
  }


  ################ Set-up ##################
  doe_decimal <- round((((doe_ratio*100)/(doe_ratio+100))/100), digits =2)
  buck_decimal <- round((1 - doe_decimal), digits = 2)


  ##### Year 0 population #####
  N_preharvest_adult_0 <- preharvest_pop * adult_prop
  N_preharvest_y1_0 <- preharvest_pop * y1_prop
  N_preharvest_yearling_0 <- preharvest_pop * yearling_prop
  N_preharvest_fawn_0 <- preharvest_pop * fawn_prop

  year_0_population <- as.data.frame(rbind(N_preharvest_adult_0, N_preharvest_y1_0, N_preharvest_yearling_0, N_preharvest_fawn_0))

  year_0_population <- data.frame(
    age_class = c("Adult", "Young", "Yearling", "Fawn"),
    total_population = c(
      N_preharvest_adult_0,
      N_preharvest_y1_0,
      N_preharvest_yearling_0,
      N_preharvest_fawn_0
    )
  )

  year_0_population$male <- round(year_0_population$total_population * buck_decimal)
  year_0_population$female <- round(year_0_population$total_population * doe_decimal)



  ##### Non-harvest mortality df #####

  nhm_df <- data.frame(
    age_class = c("adult_male_nhm", "adult_female_nhm", "yearling_male_nhm", "yearling_female_nhm", "fawn_male_nhm","fawn_female_nhm", "six_month_fawn_nhm"),
    finite_rate = c(
      adult_male_nhm,
      adult_female_nhm,
      yearling_male_nhm,
      yearling_female_nhm,
      fawn_male_nhm,
      fawn_female_nhm,
      six_month_fawn_nhm
    )
  )

  nhm_df$instantaneous_rate <- log(1- nhm_df$finite_rate)
  nhm_df$monthly_rate <- (1-exp(nhm_df$instantaneous_rate *(1/12)))


  ##### Harvest Distribution #####

  harvest_fun <- function(
    antlerless_harvest,
    antlered_harvest,
    fawn_harvest_prop,
    f_yearling_harvest_prop,
    m_yearling_harvest_prop,
    y1_prop
  ) {

    # Step 1: Fawn harvest
    fawn_female_harvest <- round(
      (antlerless_harvest * fawn_harvest_prop) / 2
    )

    fawn_male_harvest <- round(
      (antlerless_harvest * fawn_harvest_prop) / 2
    )

    # Step 2: Yearling harvest
    yearling_female_harvest <- round(
      antlerless_harvest * f_yearling_harvest_prop
    )

    yearling_male_harvest <- round(
      antlered_harvest * m_yearling_harvest_prop
    )

    # Step 3: Year 1 harvest
    y1_female_harvest <- round(
      (antlerless_harvest -
         (fawn_female_harvest +
            fawn_male_harvest +
            yearling_female_harvest)) * y1_prop
    )

    y1_male_harvest <- round(
      (antlered_harvest - yearling_male_harvest) * y1_prop
    )

    # Step 4: Adult harvest
    adult_female_harvest <- round(
      antlerless_harvest -
        (fawn_female_harvest +
           fawn_male_harvest +
           yearling_female_harvest +
           y1_female_harvest)
    )

    adult_male_harvest <- round(
      antlered_harvest -
        (yearling_male_harvest + y1_male_harvest)
    )

    # Step 5: Combine results
    harv_df <- data.frame(
      harvest_class = c(
        "adult_male_harvest",
        "adult_female_harvest",
        "y1_male_harvest",
        "y1_female_harvest",
        "yearling_male_harvest",
        "yearling_female_harvest",
        "fawn_male_harvest",
        "fawn_female_harvest"
      ),
      N_harvest = c(
        adult_male_harvest,
        adult_female_harvest,
        y1_male_harvest,
        y1_female_harvest,
        yearling_male_harvest,
        yearling_female_harvest,
        fawn_male_harvest,
        fawn_female_harvest
      )
    )

    return(harv_df)
  }

  harv_df <- harvest_fun(
    antlerless_harvest = antlerless_harvest,
    antlered_harvest = antlered_harvest,
    fawn_harvest_prop = fawn_harvest_prop,
    f_yearling_harvest_prop = f_yearling_harvest_prop,
    m_yearling_harvest_prop = m_yearling_harvest_prop,
    y1_prop = y1_prop
  )


  year_0_harvest_prep <- reshape(
    year_0_population[c("age_class", "male", "female")],
    varying = c("male", "female"),
    v.names = "population",
    timevar = "sex",
    times = c("male", "female"),
    idvar = "age_class",
    direction = "long"
  )

  rownames(year_0_harvest_prep) <- NULL

  year_0_harvest_prep <- year_0_harvest_prep %>%
    mutate(
      age_class = factor(
        age_class,
        levels = c("Adult", "Young", "Yearling", "Fawn")
      )
    ) %>%
    arrange(age_class, sex) %>%
    mutate(
      harvest_class = case_when(
        age_class == "Adult"    & sex == "male"   ~ "adult_male_harvest",
        age_class == "Adult"    & sex == "female" ~ "adult_female_harvest",
        age_class == "Young"    & sex == "male"   ~ "y1_male_harvest",
        age_class == "Young"    & sex == "female" ~ "y1_female_harvest",
        age_class == "Yearling" & sex == "male"   ~ "yearling_male_harvest",
        age_class == "Yearling" & sex == "female" ~ "yearling_female_harvest",
        age_class == "Fawn"     & sex == "male"   ~ "fawn_male_harvest",
        age_class == "Fawn"     & sex == "female" ~ "fawn_female_harvest",
        TRUE ~ NA_character_
      )
    )

  harv_df <- harv_df %>%
    left_join(
      year_0_harvest_prep %>%
        select(harvest_class, population),
      by = "harvest_class"
    )


  harv_df$precent_harvest <- harv_df$N_harvest / harv_df$population

  harv_df$precent_harvest <- round(harv_df$precent_harvest, digits = 2)



  ################ Year 1 ##################

  # Step 1
  N_adult_male_posth <- round((year_0_population[year_0_population$age_class == "Adult", "male"] - ((year_0_population[year_0_population$age_class == "Adult", "male"] - (year_0_population[year_0_population$age_class == "Adult", "male"]*(1 - ((1- nhm_df[nhm_df$age_class == "adult_male_nhm", "monthly_rate"])^(season_duration))))))*harv_df[harv_df$harvest_class == "adult_male_harvest", "precent_harvest"]) - (year_0_population[year_0_population$age_class == "Adult", "male"]*(1-((1- nhm_df[nhm_df$age_class == "adult_male_nhm", "monthly_rate"])^(season_duration)))))

  # Step 2
  N_adult_female_posth <- round((year_0_population[year_0_population$age_class == "Adult", "female"] - ((year_0_population[year_0_population$age_class == "Adult", "female"] - (year_0_population[year_0_population$age_class == "Adult", "female"]*(1 - ((1- nhm_df[nhm_df$age_class == "adult_female_nhm", "monthly_rate"])^(season_duration))))))*harv_df[harv_df$harvest_class == "adult_female_harvest", "precent_harvest"]) - (year_0_population[year_0_population$age_class == "Adult", "female"]*(1-((1- nhm_df[nhm_df$age_class == "adult_female_nhm", "monthly_rate"])^(season_duration)))))

  # Step 3
  N_y1_male_posth <- round((year_0_population[year_0_population$age_class == "Young", "male"] - ((year_0_population[year_0_population$age_class == "Young", "male"] - (year_0_population[year_0_population$age_class == "Young", "male"]*(1 - ((1- nhm_df[nhm_df$age_class == "adult_male_nhm", "monthly_rate"])^(season_duration))))))*harv_df[harv_df$harvest_class == "y1_male_harvest", "precent_harvest"]) - (year_0_population[year_0_population$age_class == "Young", "male"]*(1-((1- nhm_df[nhm_df$age_class == "adult_male_nhm", "monthly_rate"])^(season_duration)))))

  # Step 4
  N_y1_female_posth <- round((year_0_population[year_0_population$age_class == "Young", "female"] - ((year_0_population[year_0_population$age_class == "Young", "female"] - (year_0_population[year_0_population$age_class == "Young", "female"]*(1 - ((1- nhm_df[nhm_df$age_class == "adult_female_nhm", "monthly_rate"])^(season_duration))))))*harv_df[harv_df$harvest_class == "y1_female_harvest", "precent_harvest"]) - (year_0_population[year_0_population$age_class == "Young", "female"]*(1-((1- nhm_df[nhm_df$age_class == "adult_female_nhm", "monthly_rate"])^(season_duration)))))

  # Step 5
  N_yearling_male_posth <- round((year_0_population[year_0_population$age_class == "Yearling", "male"] - ((year_0_population[year_0_population$age_class == "Yearling", "male"] - (year_0_population[year_0_population$age_class == "Yearling", "male"]*(1 - ((1- nhm_df[nhm_df$age_class == "yearling_male_nhm", "monthly_rate"])^(season_duration))))))*harv_df[harv_df$harvest_class == "yearling_male_harvest", "precent_harvest"]) - (year_0_population[year_0_population$age_class == "Yearling", "male"]*(1-((1- nhm_df[nhm_df$age_class == "yearling_male_nhm", "monthly_rate"])^(season_duration)))))

  # Step 6
  N_yearling_female_posth <- round((year_0_population[year_0_population$age_class == "Yearling", "female"] - ((year_0_population[year_0_population$age_class == "Yearling", "female"] - (year_0_population[year_0_population$age_class == "Yearling", "female"]*(1 - ((1- nhm_df[nhm_df$age_class == "yearling_female_nhm", "monthly_rate"])^(season_duration))))))*harv_df[harv_df$harvest_class == "yearling_female_harvest", "precent_harvest"]) - (year_0_population[year_0_population$age_class == "Yearling", "female"]*(1-((1- nhm_df[nhm_df$age_class == "yearling_female_nhm", "monthly_rate"])^(season_duration)))))

  # Step 7
  N_fawn_male_posth <- round((year_0_population[year_0_population$age_class == "Fawn", "male"] - ((year_0_population[year_0_population$age_class == "Fawn", "male"] - (year_0_population[year_0_population$age_class == "Fawn", "male"]*(1 - ((1- nhm_df[nhm_df$age_class == "fawn_male_nhm", "monthly_rate"])^(season_duration))))))*harv_df[harv_df$harvest_class == "fawn_male_harvest", "precent_harvest"]) - (year_0_population[year_0_population$age_class == "Fawn", "male"]*(1-((1- nhm_df[nhm_df$age_class == "fawn_male_nhm", "monthly_rate"])^(season_duration)))))

  # Step 8
  N_fawn_female_posth <-  round((year_0_population[year_0_population$age_class == "Fawn", "female"] - ((year_0_population[year_0_population$age_class == "Fawn", "female"] - (year_0_population[year_0_population$age_class == "Fawn", "female"]*(1 - ((1- nhm_df[nhm_df$age_class == "fawn_female_nhm", "monthly_rate"])^(season_duration))))))*harv_df[harv_df$harvest_class == "fawn_female_harvest", "precent_harvest"]) - (year_0_population[year_0_population$age_class == "Fawn", "female"]*(1-((1- nhm_df[nhm_df$age_class == "fawn_female_nhm", "monthly_rate"])^(season_duration)))))

  # Step 9
  N_postharvest <- N_adult_male_posth + N_adult_female_posth + N_y1_male_posth + N_y1_female_posth + N_yearling_male_posth + N_yearling_female_posth + N_fawn_male_posth + N_fawn_female_posth



  ### MidYear ###

  # Step 1
  N_adult_male_mid <- round((N_adult_male_posth + N_y1_male_posth) - ((N_adult_male_posth + N_y1_male_posth)* (1 - ((1- nhm_df[nhm_df$age_class == "adult_male_nhm", "monthly_rate"])^(12 - (season_open +(season_duration - 1)) + birth_month)))))

  # Step 2
  N_adult_female_mid <- round((N_adult_female_posth + N_y1_female_posth) - ((N_adult_female_posth + N_y1_female_posth)* (1 - ((1- nhm_df[nhm_df$age_class == "adult_female_nhm", "monthly_rate"])^(12 - (season_open +(season_duration - 1)) + birth_month)))))

  # Step 3
  N_y1_male_mid <- round(N_yearling_male_posth - (N_yearling_male_posth * (1 - ((1- nhm_df[nhm_df$age_class == "yearling_male_nhm", "monthly_rate"])^(12 - (season_open +(season_duration - 1)) + birth_month)))))

  # Step 4
  N_y1_female_mid <- round(N_yearling_female_posth - (N_yearling_female_posth * (1 - ((1- nhm_df[nhm_df$age_class == "yearling_female_nhm", "monthly_rate"])^(12 - (season_open +(season_duration - 1)) + birth_month)))))

  # Step 5
  N_yearling_male_mid <- round(N_fawn_male_posth - (N_fawn_male_posth * (1 - ((1- nhm_df[nhm_df$age_class == "fawn_male_nhm", "monthly_rate"])^(12 - (season_open +(season_duration - 1)) + birth_month)))))

  # Step 6
  N_yearling_female_mid <- round(N_fawn_female_posth - (N_fawn_female_posth * (1 - ((1- nhm_df[nhm_df$age_class == "fawn_female_nhm", "monthly_rate"])^(12 - (season_open +(season_duration - 1)) + birth_month)))))

  # Step 7
  birth_male_fawn <- round(((N_adult_female_mid*adult_fawn_rate) + (N_y1_female_mid*adult_fawn_rate) + (N_yearling_female_mid*yearling_fawn_rate))/2)

  # Step 8
  birth_female_fawn <- round(((N_adult_female_mid*adult_fawn_rate) + (N_y1_female_mid*adult_fawn_rate) + (N_yearling_female_mid*yearling_fawn_rate))/2)

  # Step 9
  N_midyear <- N_adult_male_mid +
    N_adult_female_mid +
    N_y1_male_mid +
    N_y1_female_mid +
    N_yearling_male_mid +
    N_yearling_female_mid +
    birth_male_fawn +
    birth_female_fawn


  ### Preharvest ###

  # Step 1
  N_adult_male_preh <- round(N_adult_male_mid - (N_adult_male_mid* (1-((1-nhm_df[nhm_df$age_class == "adult_male_nhm", "monthly_rate"])^((season_open-1)- birth_month)))))

  # Step 2
  N_adult_female_preh <- round(N_adult_female_mid - (N_adult_female_mid* (1-((1-nhm_df[nhm_df$age_class == "adult_female_nhm", "monthly_rate"])^((season_open-1)- birth_month)))))

  # Step 3
  N_y1_male_preh <- round(N_y1_male_mid - (N_y1_male_mid* (1-((1-nhm_df[nhm_df$age_class == "adult_male_nhm", "monthly_rate"])^((season_open-1)- birth_month)))))

  # Step 4
  N_y1_female_preh <- round(N_y1_female_mid - (N_y1_female_mid* (1-((1-nhm_df[nhm_df$age_class == "adult_female_nhm", "monthly_rate"])^((season_open-1)- birth_month)))))

  # Step 5
  N_yearling_male_preh <- round(N_yearling_male_mid - (N_yearling_male_mid* (1-((1-nhm_df[nhm_df$age_class == "yearling_male_nhm", "monthly_rate"])^((season_open-1)- birth_month)))))

  # Step 6
  N_yearling_female_preh <- round(N_yearling_female_mid - (N_yearling_female_mid* (1-((1-nhm_df[nhm_df$age_class == "yearling_female_nhm", "monthly_rate"])^((season_open-1)- birth_month)))))

  # Step 7
  N_fawn_male_preh <- round(birth_male_fawn - (birth_male_fawn* (1-((1-nhm_df[nhm_df$age_class == "six_month_fawn_nhm", "monthly_rate"])^((season_open-1)- birth_month)))))

  # Step 8
  N_fawn_female_preh <- round(birth_female_fawn - (birth_female_fawn* (1-((1-nhm_df[nhm_df$age_class == "six_month_fawn_nhm", "monthly_rate"])^((season_open-1)- birth_month)))))

  # Step 9
  N_preharvest <-  N_adult_male_preh +
    N_adult_female_preh +
    N_y1_male_preh +
    N_y1_female_preh +
    N_yearling_male_preh +
    N_yearling_female_preh +
    N_fawn_male_preh +
    N_fawn_female_preh

  ### Modeled Harvest ###

  # Step 1
  N_adult_male_mharvest <- round(N_adult_male_preh*harv_df[harv_df$harvest_class == "adult_male_harvest", "precent_harvest"])

  # Step 2
  N_adult_female_mharvest <- round(N_adult_female_preh*harv_df[harv_df$harvest_class == "adult_female_harvest", "precent_harvest"])

  # Step 3
  N_y1_male_mharvest <- round(N_y1_male_preh*harv_df[harv_df$harvest_class == "y1_male_harvest", "precent_harvest"])

  # Step 4
  N_y1_female_mharvest <- round(N_y1_female_preh*harv_df[harv_df$harvest_class == "y1_female_harvest", "precent_harvest"])

  # Step 5
  N_yearling_male_mharvest <- round(N_yearling_male_preh*harv_df[harv_df$harvest_class == "yearling_male_harvest", "precent_harvest"])

  # Step 6
  N_yearling_female_mharvest <- round(N_yearling_female_preh*harv_df[harv_df$harvest_class == "yearling_female_harvest", "precent_harvest"])

  # Step 7
  N_fawn_male_mharvest <- round(N_fawn_male_preh*harv_df[harv_df$harvest_class == "fawn_male_harvest", "precent_harvest"])

  # Step 8
  N_fawn_female_mharvest <- round(N_fawn_female_preh*harv_df[harv_df$harvest_class == "fawn_female_harvest", "precent_harvest"])

  # Step 9
  N_mharvest <-  N_adult_male_mharvest +
    N_adult_female_mharvest +
    N_y1_male_mharvest +
    N_y1_female_mharvest +
    N_yearling_male_mharvest +
    N_yearling_female_mharvest +
    N_fawn_male_mharvest +
    N_fawn_female_mharvest

  # Combine all results into one data frame
  previous_year <- data.frame(
    N_adult_male_posth,
    N_adult_female_posth,
    N_y1_male_posth,
    N_y1_female_posth,
    N_yearling_male_posth,
    N_yearling_female_posth,
    N_fawn_male_posth,
    N_fawn_female_posth,
    N_postharvest,
    N_adult_male_mid,
    N_adult_female_mid,
    N_y1_male_mid,
    N_y1_female_mid,
    N_yearling_male_mid,
    N_yearling_female_mid,
    birth_male_fawn,
    birth_female_fawn,
    N_midyear,
    N_adult_male_preh,
    N_adult_female_preh,
    N_y1_male_preh,
    N_y1_female_preh,
    N_yearling_male_preh,
    N_yearling_female_preh,
    N_fawn_male_preh,
    N_fawn_female_preh,
    N_preharvest,
    N_adult_male_mharvest,
    N_adult_female_mharvest,
    N_y1_male_mharvest,
    N_y1_female_mharvest,
    N_yearling_male_mharvest,
    N_yearling_female_mharvest,
    N_fawn_male_mharvest,
    N_fawn_female_mharvest,
    N_mharvest
  )

  ########### Beyond year 1 ###############
  year_fun <- function(
    previous_year,
    season_duration,
    season_open,
    birth_month,
    adult_fawn_rate,
    yearling_fawn_rate,
    hc_adult_male,
    hc_adult_female,
    hc_y1_male,
    hc_y1_female,
    hc_yearling_male,
    hc_yearling_female,
    hc_fawn_male,
    hc_fawn_female,
    adm_adult_male,
    adm_adult_female,
    adm_y1_male,
    adm_y1_female,
    adm_yearling_male,
    adm_yearling_female,
    adm_fawn_male,
    adm_fawn_female
  ) {

    ### Postharvest ###

    N_adult_male_posth <- round(
      previous_year$N_adult_male_preh -
        (
          previous_year$N_adult_male_preh -
            (
              previous_year$N_adult_male_preh *
                (1 - ((1 - nhm_df[nhm_df$age_class == "adult_male_nhm",
                                  "monthly_rate"])^season_duration))
            )
        ) *
        (
          harv_df[harv_df$harvest_class == "adult_male_harvest",
                  "precent_harvest"] *
            (1 + hc_adult_male)
        ) -
        (
          previous_year$N_adult_male_preh *
            (1 - ((1 - nhm_df[nhm_df$age_class == "adult_male_nhm",
                              "monthly_rate"])^season_duration))
        )
    )

    N_adult_female_posth <- round(
      previous_year$N_adult_female_preh -
        (
          previous_year$N_adult_female_preh -
            (
              previous_year$N_adult_female_preh *
                (1 - ((1 - nhm_df[nhm_df$age_class == "adult_female_nhm",
                                  "monthly_rate"])^season_duration))
            )
        ) *
        (
          harv_df[harv_df$harvest_class == "adult_female_harvest",
                  "precent_harvest"] *
            (1 + hc_adult_female)
        ) -
        (
          previous_year$N_adult_female_preh *
            (1 - ((1 - nhm_df[nhm_df$age_class == "adult_female_nhm",
                              "monthly_rate"])^season_duration))
        )
    )

    N_y1_male_posth <- round(
      previous_year$N_y1_male_preh -
        (
          previous_year$N_y1_male_preh -
            (
              previous_year$N_y1_male_preh *
                (1 - ((1 - nhm_df[nhm_df$age_class == "adult_male_nhm",
                                  "monthly_rate"])^season_duration))
            )
        ) *
        (
          harv_df[harv_df$harvest_class == "y1_male_harvest",
                  "precent_harvest"] *
            (1 + hc_y1_male)
        ) -
        (
          previous_year$N_y1_male_preh *
            (1 - ((1 - nhm_df[nhm_df$age_class == "adult_male_nhm",
                              "monthly_rate"])^season_duration))
        )
    )

    N_y1_female_posth <- round(
      previous_year$N_y1_female_preh -
        (
          previous_year$N_y1_female_preh -
            (
              previous_year$N_y1_female_preh *
                (1 - ((1 - nhm_df[nhm_df$age_class == "adult_female_nhm",
                                  "monthly_rate"])^season_duration))
            )
        ) *
        (
          harv_df[harv_df$harvest_class == "y1_female_harvest",
                  "precent_harvest"] *
            (1 + hc_y1_female)
        ) -
        (
          previous_year$N_y1_female_preh *
            (1 - ((1 - nhm_df[nhm_df$age_class == "adult_female_nhm",
                              "monthly_rate"])^season_duration))
        )
    )

    N_yearling_male_posth <- round(
      previous_year$N_yearling_male_preh -
        (
          previous_year$N_yearling_male_preh -
            (
              previous_year$N_yearling_male_preh *
                (1 - ((1 - nhm_df[nhm_df$age_class == "yearling_male_nhm",
                                  "monthly_rate"])^season_duration))
            )
        ) *
        (
          harv_df[harv_df$harvest_class == "yearling_male_harvest",
                  "precent_harvest"] *
            (1 + hc_yearling_male)
        ) -
        (
          previous_year$N_yearling_male_preh *
            (1 - ((1 - nhm_df[nhm_df$age_class == "yearling_male_nhm",
                              "monthly_rate"])^season_duration))
        )
    )

    N_yearling_female_posth <- round(
      previous_year$N_yearling_female_preh -
        (
          previous_year$N_yearling_female_preh -
            (
              previous_year$N_yearling_female_preh *
                (1 - ((1 - nhm_df[nhm_df$age_class == "yearling_female_nhm",
                                  "monthly_rate"])^season_duration))
            )
        ) *
        (
          harv_df[harv_df$harvest_class == "yearling_female_harvest",
                  "precent_harvest"] *
            (1 + hc_yearling_female)
        ) -
        (
          previous_year$N_yearling_female_preh *
            (1 - ((1 - nhm_df[nhm_df$age_class == "yearling_female_nhm",
                              "monthly_rate"])^season_duration))
        )
    )

    N_fawn_male_posth <- round(
      previous_year$N_fawn_male_preh -
        (
          previous_year$N_fawn_male_preh -
            (
              previous_year$N_fawn_male_preh *
                (1 - ((1 - nhm_df[nhm_df$age_class == "fawn_male_nhm",
                                  "monthly_rate"])^season_duration))
            )
        ) *
        (
          harv_df[harv_df$harvest_class == "fawn_male_harvest",
                  "precent_harvest"] *
            (1 + hc_fawn_male)
        ) -
        (
          previous_year$N_fawn_male_preh *
            (1 - ((1 - nhm_df[nhm_df$age_class == "fawn_male_nhm",
                              "monthly_rate"])^season_duration))
        )
    )

    N_fawn_female_posth <- round(
      previous_year$N_fawn_female_preh -
        (
          previous_year$N_fawn_female_preh -
            (
              previous_year$N_fawn_female_preh *
                (1 - ((1 - nhm_df[nhm_df$age_class == "fawn_female_nhm",
                                  "monthly_rate"])^season_duration))
            )
        ) *
        (
          harv_df[harv_df$harvest_class == "fawn_female_harvest",
                  "precent_harvest"] *
            (1 + hc_fawn_female)
        ) -
        (
          previous_year$N_fawn_female_preh *
            (1 - ((1 - nhm_df[nhm_df$age_class == "fawn_female_nhm",
                              "monthly_rate"])^season_duration))
        )
    )

    N_postharvest <-
      N_adult_male_posth +
      N_adult_female_posth +
      N_y1_male_posth +
      N_y1_female_posth +
      N_yearling_male_posth +
      N_yearling_female_posth +
      N_fawn_male_posth +
      N_fawn_female_posth


    ### MidYear ###

    N_adult_male_mid <- round(
      (N_adult_male_posth + N_y1_male_posth) -
        ((N_adult_male_posth + N_y1_male_posth) *
           (1 - ((1 - nhm_df[nhm_df$age_class == "adult_male_nhm",
                             "monthly_rate"])^(
                               12 - (season_open + (season_duration - 1)) +
                                 birth_month))))
    )

    N_adult_female_mid <- round(
      (N_adult_female_posth + N_y1_female_posth) -
        ((N_adult_female_posth + N_y1_female_posth) *
           (1 - ((1 - nhm_df[nhm_df$age_class == "adult_female_nhm",
                             "monthly_rate"])^(
                               12 - (season_open + (season_duration - 1)) +
                                 birth_month))))
    )

    N_y1_male_mid <- round(
      N_yearling_male_posth -
        (N_yearling_male_posth *
           (1 - ((1 - nhm_df[nhm_df$age_class == "yearling_male_nhm",
                             "monthly_rate"])^(
                               12 - (season_open + (season_duration - 1)) +
                                 birth_month))))
    )

    N_y1_female_mid <- round(
      N_yearling_female_posth -
        (N_yearling_female_posth *
           (1 - ((1 - nhm_df[nhm_df$age_class == "yearling_female_nhm",
                             "monthly_rate"])^(
                               12 - (season_open + (season_duration - 1)) +
                                 birth_month))))
    )

    N_yearling_male_mid <- round(
      N_fawn_male_posth -
        (N_fawn_male_posth *
           (1 - ((1 - nhm_df[nhm_df$age_class == "fawn_male_nhm",
                             "monthly_rate"])^(
                               12 - (season_open + (season_duration - 1)) +
                                 birth_month))))
    )

    N_yearling_female_mid <- round(
      N_fawn_female_posth -
        (N_fawn_female_posth *
           (1 - ((1 - nhm_df[nhm_df$age_class == "fawn_female_nhm",
                             "monthly_rate"])^(
                               12 - (season_open + (season_duration - 1)) +
                                 birth_month))))
    )

    birth_male_fawn <- round(
      (
        (N_adult_female_mid * adult_fawn_rate) +
          (N_y1_female_mid * adult_fawn_rate) +
          (N_yearling_female_mid * yearling_fawn_rate)
      ) / 2
    )

    birth_female_fawn <- round(
      (
        (N_adult_female_mid * adult_fawn_rate) +
          (N_y1_female_mid * adult_fawn_rate) +
          (N_yearling_female_mid * yearling_fawn_rate)
      ) / 2
    )

    N_midyear <-
      N_adult_male_mid +
      N_adult_female_mid +
      N_y1_male_mid +
      N_y1_female_mid +
      N_yearling_male_mid +
      N_yearling_female_mid +
      birth_male_fawn +
      birth_female_fawn


    ### Preharvest ###

    N_adult_male_preh <- round(
      (N_adult_male_mid -
         N_adult_male_mid *
         (1 - ((1 - nhm_df[nhm_df$age_class == "adult_male_nhm",
                           "monthly_rate"])^((season_open - 1) - birth_month)))) -
        (
          (
            N_adult_male_mid -
              N_adult_male_mid *
              (1 - ((1 - nhm_df[nhm_df$age_class == "adult_male_nhm",
                                "monthly_rate"])^((season_open - 1) -
                                                    birth_month)))
          ) * adm_adult_male
        )
    )

    N_adult_female_preh <- round(
      (N_adult_female_mid -
         N_adult_female_mid *
         (1 - ((1 - nhm_df[nhm_df$age_class == "adult_female_nhm",
                           "monthly_rate"])^((season_open - 1) - birth_month)))) -
        (
          (
            N_adult_female_mid -
              N_adult_female_mid *
              (1 - ((1 - nhm_df[nhm_df$age_class == "adult_female_nhm",
                                "monthly_rate"])^((season_open - 1) -
                                                    birth_month)))
          ) * adm_adult_female
        )
    )

    N_y1_male_preh <- round(
      (N_y1_male_mid -
         N_y1_male_mid *
         (1 - ((1 - nhm_df[nhm_df$age_class == "adult_male_nhm",
                           "monthly_rate"])^((season_open - 1) - birth_month)))) -
        (
          (
            N_y1_male_mid -
              N_y1_male_mid *
              (1 - ((1 - nhm_df[nhm_df$age_class == "adult_male_nhm",
                                "monthly_rate"])^((season_open - 1) -
                                                    birth_month)))
          ) * adm_y1_male
        )
    )

    N_y1_female_preh <- round(
      (N_y1_female_mid -
         N_y1_female_mid *
         (1 - ((1 - nhm_df[nhm_df$age_class == "adult_female_nhm",
                           "monthly_rate"])^((season_open - 1) - birth_month)))) -
        (
          (
            N_y1_female_mid -
              N_y1_female_mid *
              (1 - ((1 - nhm_df[nhm_df$age_class == "adult_female_nhm",
                                "monthly_rate"])^((season_open - 1) -
                                                    birth_month)))
          ) * adm_y1_female
        )
    )

    N_yearling_male_preh <- round(
      (N_yearling_male_mid -
         N_yearling_male_mid *
         (1 - ((1 - nhm_df[nhm_df$age_class == "yearling_male_nhm",
                           "monthly_rate"])^((season_open - 1) - birth_month)))) -
        (
          (
            N_yearling_male_mid -
              N_yearling_male_mid *
              (1 - ((1 - nhm_df[nhm_df$age_class == "yearling_male_nhm",
                                "monthly_rate"])^((season_open - 1) -
                                                    birth_month)))
          ) * adm_yearling_male
        )
    )

    N_yearling_female_preh <- round(
      (N_yearling_female_mid -
         N_yearling_female_mid *
         (1 - ((1 - nhm_df[nhm_df$age_class == "yearling_female_nhm",
                           "monthly_rate"])^((season_open - 1) - birth_month)))) -
        (
          (
            N_yearling_female_mid -
              N_yearling_female_mid *
              (1 - ((1 - nhm_df[nhm_df$age_class == "yearling_female_nhm",
                                "monthly_rate"])^((season_open - 1) -
                                                    birth_month)))
          ) * adm_yearling_female
        )
    )

    N_fawn_male_preh <- round(
      (birth_male_fawn -
         birth_male_fawn *
         (1 - ((1 - nhm_df[nhm_df$age_class == "six_month_fawn_nhm",
                           "monthly_rate"])^((season_open - 1) - birth_month)))) -
        (
          (
            birth_male_fawn -
              birth_male_fawn *
              (1 - ((1 - nhm_df[nhm_df$age_class == "six_month_fawn_nhm",
                                "monthly_rate"])^((season_open - 1) -
                                                    birth_month)))
          ) * adm_fawn_male
        )
    )

    N_fawn_female_preh <- round(
      (birth_female_fawn -
         birth_female_fawn *
         (1 - ((1 - nhm_df[nhm_df$age_class == "six_month_fawn_nhm",
                           "monthly_rate"])^((season_open - 1) - birth_month)))) -
        (
          (
            birth_female_fawn -
              birth_female_fawn *
              (1 - ((1 - nhm_df[nhm_df$age_class == "six_month_fawn_nhm",
                                "monthly_rate"])^((season_open - 1) -
                                                    birth_month)))
          ) * adm_fawn_female
        )
    )

    N_preharvest <-
      N_adult_male_preh +
      N_adult_female_preh +
      N_y1_male_preh +
      N_y1_female_preh +
      N_yearling_male_preh +
      N_yearling_female_preh +
      N_fawn_male_preh +
      N_fawn_female_preh


    ### Harvest ###

    N_adult_male_mharvest <- round(
      N_adult_male_preh *
        (
          harv_df[harv_df$harvest_class == "adult_male_harvest",
                  "precent_harvest"] *
            (1 + hc_adult_male)
        )
    )

    N_adult_female_mharvest <- round(
      N_adult_female_preh *
        (
          harv_df[harv_df$harvest_class == "adult_female_harvest",
                  "precent_harvest"] *
            (1 + hc_adult_female)
        )
    )

    N_y1_male_mharvest <- round(
      N_y1_male_preh *
        (
          harv_df[harv_df$harvest_class == "y1_male_harvest",
                  "precent_harvest"] *
            (1 + hc_y1_male)
        )
    )

    N_y1_female_mharvest <- round(
      N_y1_female_preh *
        (
          harv_df[harv_df$harvest_class == "y1_female_harvest",
                  "precent_harvest"] *
            (1 + hc_y1_female)
        )
    )

    N_yearling_male_mharvest <- round(
      N_yearling_male_preh *
        (
          harv_df[harv_df$harvest_class == "yearling_male_harvest",
                  "precent_harvest"] *
            (1 + hc_yearling_male)
        )
    )

    N_yearling_female_mharvest <- round(
      N_yearling_female_preh *
        (
          harv_df[harv_df$harvest_class == "yearling_female_harvest",
                  "precent_harvest"] *
            (1 + hc_yearling_female)
        )
    )

    N_fawn_male_mharvest <- round(
      N_fawn_male_preh *
        (
          harv_df[harv_df$harvest_class == "fawn_male_harvest",
                  "precent_harvest"] *
            (1 + hc_fawn_male)
        )
    )

    N_fawn_female_mharvest <- round(
      N_fawn_female_preh *
        (
          harv_df[harv_df$harvest_class == "fawn_female_harvest",
                  "precent_harvest"] *
            (1 + hc_fawn_female)
        )
    )

    N_mharvest <-
      N_adult_male_mharvest +
      N_adult_female_mharvest +
      N_y1_male_mharvest +
      N_y1_female_mharvest +
      N_yearling_male_mharvest +
      N_yearling_female_mharvest +
      N_fawn_male_mharvest +
      N_fawn_female_mharvest


    ### Return one year ###

    data.frame(
      N_adult_male_posth = N_adult_male_posth,
      N_adult_female_posth = N_adult_female_posth,
      N_y1_male_posth = N_y1_male_posth,
      N_y1_female_posth = N_y1_female_posth,
      N_yearling_male_posth = N_yearling_male_posth,
      N_yearling_female_posth = N_yearling_female_posth,
      N_fawn_male_posth = N_fawn_male_posth,
      N_fawn_female_posth = N_fawn_female_posth,
      N_postharvest = N_postharvest,
      N_adult_male_mid = N_adult_male_mid,
      N_adult_female_mid = N_adult_female_mid,
      N_y1_male_mid = N_y1_male_mid,
      N_y1_female_mid = N_y1_female_mid,
      N_yearling_male_mid = N_yearling_male_mid,
      N_yearling_female_mid = N_yearling_female_mid,
      birth_male_fawn = birth_male_fawn,
      birth_female_fawn = birth_female_fawn,
      N_midyear = N_midyear,
      N_adult_male_preh = N_adult_male_preh,
      N_adult_female_preh = N_adult_female_preh,
      N_y1_male_preh = N_y1_male_preh,
      N_y1_female_preh = N_y1_female_preh,
      N_yearling_male_preh = N_yearling_male_preh,
      N_yearling_female_preh = N_yearling_female_preh,
      N_fawn_male_preh = N_fawn_male_preh,
      N_fawn_female_preh = N_fawn_female_preh,
      N_preharvest = N_preharvest,
      N_adult_male_mharvest = N_adult_male_mharvest,
      N_adult_female_mharvest = N_adult_female_mharvest,
      N_y1_male_mharvest = N_y1_male_mharvest,
      N_y1_female_mharvest = N_y1_female_mharvest,
      N_yearling_male_mharvest = N_yearling_male_mharvest,
      N_yearling_female_mharvest = N_yearling_female_mharvest,
      N_fawn_male_mharvest = N_fawn_male_mharvest,
      N_fawn_female_mharvest = N_fawn_female_mharvest,
      N_mharvest = N_mharvest
    )
  }


  ############# Loop #################

  # Store all 15 years
  results <- list()

  results$ModelYear <- 1:project_years

  # Year 1 is already calculated above
  results[[1]] <- previous_year


  # Years 2 through 15
  for (i in 2:project_years) {

    # Determine whether harvest change is active
    if (length(hc_specify) > 0 &&
        !all(hc_specify == 0)) {

      # Explicit ModelYears override hc_duration
      harvest_active <- i %in% hc_specify

    } else {

      # Use hc_duration when no specific years are supplied
      harvest_active <- i >= 4 &&
        i <= (3 + hc_duration)
    }

    if (harvest_active) {

      hc_adult_male2     <- hc_adult_male
      hc_adult_female2   <- hc_adult_female
      hc_y1_male2     <- hc_y1_male
      hc_y1_female2   <- hc_y1_female
      hc_yearling_male2      <- hc_yearling_male
      hc_yearling_female2    <- hc_yearling_female
      hc_fawn_male2   <- hc_fawn_male
      hc_fawn_female2 <- hc_fawn_female

    } else {

      hc_adult_male2     <- 0
      hc_adult_female2   <- 0
      hc_y1_male2     <- 0
      hc_y1_female2   <- 0
      hc_yearling_male2      <- 0
      hc_yearling_female2    <- 0
      hc_fawn_male2   <- 0
      hc_fawn_female2 <- 0
    }

    # Additional mortality change

    if (length(adm_specify) > 0 &&
        !all(adm_specify == 0)) {

      # Explicitly specified ModelYears
      adm_active <- i %in% adm_specify

    } else {

      # Duration-based approach
      adm_active <- i >= 4 &&
        i <= (3 + adm_duration)
    }


    if (adm_active) {

      adm_adult_male2     <- adm_adult_male
      adm_adult_female2   <- adm_adult_female
      adm_y1_male2     <- adm_y1_male
      adm_y1_female2   <- adm_y1_female
      adm_yearling_male2      <- adm_yearling_male
      adm_yearling_female2    <- adm_yearling_female
      adm_fawn_male2   <- adm_fawn_male
      adm_fawn_female2 <- adm_fawn_female

    } else {

      adm_adult_male2     <- 0
      adm_adult_female2   <- 0
      adm_y1_male2     <- 0
      adm_y1_female2   <- 0
      adm_yearling_male2      <- 0
      adm_yearling_female2    <- 0
      adm_fawn_male2   <- 0
      adm_fawn_female2 <- 0
    }


    #Running next year

    results[[i]] <- year_fun(

      previous_year = results[[i - 1]],

      season_duration = season_duration,
      season_open = season_open,
      birth_month = birth_month,
      adult_fawn_rate = adult_fawn_rate,
      yearling_fawn_rate = yearling_fawn_rate,

      hc_adult_male = hc_adult_male2,
      hc_adult_female = hc_adult_female2,
      hc_y1_male = hc_y1_male2,
      hc_y1_female = hc_y1_female2,
      hc_yearling_male = hc_yearling_male2,
      hc_yearling_female = hc_yearling_female2,
      hc_fawn_male = hc_fawn_male2,
      hc_fawn_female = hc_fawn_female2,

      adm_adult_male = adm_adult_male2,
      adm_adult_female = adm_adult_female2,
      adm_y1_male = adm_y1_male2,
      adm_y1_female = adm_y1_female2,
      adm_yearling_male = adm_yearling_male2,
      adm_yearling_female = adm_yearling_female2,
      adm_fawn_male = adm_fawn_male2,
      adm_fawn_female = adm_fawn_female2
    )
  }

  results_full <- do.call(
    rbind,
    results
  )

  # Reset row names
  rownames(results_full) <- NULL


  # Add model year
  results_full$ModelYear <- 1:project_years

  # Add Lambda
  results_full$Lambda <- NA_real_

  results_full$Lambda[2:project_years] <-
    results_full$N_preharvest[2:project_years] /
    results_full$N_preharvest[1:(project_years - 1)]

  results_full$Lambda <- round(results_full$Lambda, digits = 2)

  # Move ModelYear to first column
  results_full <- results_full[, c(
    "ModelYear",
    setdiff(names(results_full), "ModelYear")
  )]

  return(as.data.frame(results_full))

}
