#' Project deer population dynamics
#'
#' Projects deer population dynamics over 15 model years.
#'
#' @return A data frame containing the population projection.
#' @export


AcCountDeer <- function(
    PreharvestPop,
    DoeRat,
    adProp,
    y1Prop,
    yearProp,
    fawnProp,
    adFawnRate,
    yearFawnRate,
    antleredHarv,
    antlerlessHarv,
    yearHarvProp_M,
    yearHarvProp_F,
    fawnHarvProp,
    birthMonth,
    seasonOpen,
    seasonDuration,
    admale_nhm,
    adfemale_nhm,
    yearmale_nhm,
    yearfemale_nhm,
    fawnmale_nhm,
    fawnfemale_nhm,
    sixmonthfawn_nhm,

    harvest_chng_admale,
    harvest_chng_adfemale,
    harvest_chng_y1male,
    harvest_chng_y1female,
    harvest_chng_ymale,
    harvest_chng_yfemale,
    harvest_chng_fawnmale,
    harvest_chng_fawnfemale,

    adMort_admale,
    adMort_adfemale,
    adMort_y1male,
    adMort_y1female,
    adMort_ymale,
    adMort_yfemale,
    adMort_fawnmale,
    adMort_fawnfemale,

    changeDuration,
    adMortChangeDuration,

    iterative = FALSE
) {


  ################ Set-up ##################
  Doe_Decimal <- round((((DoeRat*100)/(DoeRat+100))/100), digits =2)
  Buck_Decimal <- round((1 - Doe_Decimal), digits = 2)


  ##### Year 0 population #####
  N_preharvestAd_0 <- PreharvestPop * adProp
  N_preharvesty1_0 <- PreharvestPop * y1Prop
  N_preharvestyear_0 <- PreharvestPop * yearProp
  N_preharvestfawn_0 <- PreharvestPop * fawnProp

  year0population <- as.data.frame(rbind(N_preharvestAd_0, N_preharvesty1_0, N_preharvestyear_0, N_preharvestfawn_0))

  year0population <- data.frame(
    AgeClass = c("Adult", "Young", "Yearling", "Fawn"),
    TotalPopulation = c(
      N_preharvestAd_0,
      N_preharvesty1_0,
      N_preharvestyear_0,
      N_preharvestfawn_0
    )
  )

  year0population$male <- round(year0population$TotalPopulation * Buck_Decimal)
  year0population$female <- round(year0population$TotalPopulation * Doe_Decimal)



  ##### Non-harvest mortality df #####

  nhm_df <- data.frame(
    AgeClass = c("admale_nhm", "adfemale_nhm", "yearmale_nhm", "yearfemale_nhm", "fawnmale_nhm","fawnfemale_nhm", "sixmonthfawn_nhm"),
    finiteRate = c(
      admale_nhm,
      adfemale_nhm,
      yearmale_nhm,
      yearfemale_nhm,
      fawnmale_nhm,
      fawnfemale_nhm,
      sixmonthfawn_nhm
    )
  )

  nhm_df$instantaneousRate <- log(1- nhm_df$finiteRate)
  nhm_df$monthlyRate <- (1-exp(nhm_df$instantaneousRate *(1/12)))


  ##### Harvest Distribution #####

  harvest_fun <- function(
    antlerlessHarv,
    antleredHarv,
    fawnHarvProp,
    yearHarvProp_F,
    yearHarvProp_M,
    y1Prop
  ) {

    # Step 1: Fawn harvest
    fawnfemale_harv <- round(
      (antlerlessHarv * fawnHarvProp) / 2
    )

    fawnmale_harv <- round(
      (antlerlessHarv * fawnHarvProp) / 2
    )

    # Step 2: Yearling harvest
    yearfemale_harv <- round(
      antlerlessHarv * yearHarvProp_F
    )

    yearmale_harv <- round(
      antleredHarv * yearHarvProp_M
    )

    # Step 3: Year 1 harvest
    y1female_harv <- round(
      (antlerlessHarv -
         (fawnfemale_harv +
            fawnmale_harv +
            yearfemale_harv)) * y1Prop
    )

    y1male_harv <- round(
      (antleredHarv - yearmale_harv) * y1Prop
    )

    # Step 4: Adult harvest
    adfemale_harv <- round(
      antlerlessHarv -
        (fawnfemale_harv +
           fawnmale_harv +
           yearfemale_harv +
           y1female_harv)
    )

    admale_harv <- round(
      antleredHarv -
        (yearmale_harv + y1male_harv)
    )

    # Step 5: Combine results
    harv_df <- data.frame(
      harvestClass = c(
        "admale_harv",
        "adfemale_harv",
        "y1male_harv",
        "y1female_harv",
        "yearmale_harv",
        "yearfemale_harv",
        "fawnmale_harv",
        "fawnfemale_harv"
      ),
      N_Harvest = c(
        admale_harv,
        adfemale_harv,
        y1male_harv,
        y1female_harv,
        yearmale_harv,
        yearfemale_harv,
        fawnmale_harv,
        fawnfemale_harv
      )
    )

    return(harv_df)
  }

  harv_df <- harvest_fun(
    antlerlessHarv = antlerlessHarv,
    antleredHarv = antleredHarv,
    fawnHarvProp = fawnHarvProp,
    yearHarvProp_F = yearHarvProp_F,
    yearHarvProp_M = yearHarvProp_M,
    y1Prop = y1Prop
  )


  year0_harvprep <- reshape2::melt(
    data = dplyr::select(year0population, -TotalPopulation),
    id.vars = "AgeClass",
    variable.name = "Sex",
    value.name = "Population"
  )

  year0_harvprep <- year0_harvprep |>
    dplyr::mutate(
      AgeClass = factor(
        AgeClass,
        levels = c("Adult", "Young", "Yearling", "Fawn")
      )
    ) |>
    dplyr::arrange(AgeClass, Sex) |>
    dplyr::mutate(
      harvestClass = dplyr::case_when(
        AgeClass == "Adult"    & Sex == "male"   ~ "admale_harv",
        AgeClass == "Adult"    & Sex == "female" ~ "adfemale_harv",
        AgeClass == "Young"    & Sex == "male"   ~ "y1male_harv",
        AgeClass == "Young"    & Sex == "female" ~ "y1female_harv",
        AgeClass == "Yearling" & Sex == "male"   ~ "yearmale_harv",
        AgeClass == "Yearling" & Sex == "female" ~ "yearfemale_harv",
        AgeClass == "Fawn"     & Sex == "male"   ~ "fawnmale_harv",
        AgeClass == "Fawn"     & Sex == "female" ~ "fawnfemale_harv",
        TRUE ~ NA_character_
      )
    )

  harv_df <- harv_df |>
    dplyr::left_join(
      year0_harvprep |>
        dplyr::select(harvestClass, Population),
      by = "harvestClass"
    )


  harv_df$Precent_harv <- harv_df$N_Harvest / harv_df$Population

  harv_df$Precent_harv <- round(harv_df$Precent_harv, digits = 2)



  ################ Year 1 ##################

  # Step 1
  N_admale_posth <- round((year0population[year0population$AgeClass == "Adult", "male"] - ((year0population[year0population$AgeClass == "Adult", "male"] - (year0population[year0population$AgeClass == "Adult", "male"]*(1 - ((1- nhm_df[nhm_df$AgeClass == "admale_nhm", "monthlyRate"])^(seasonDuration))))))*harv_df[harv_df$harvestClass == "admale_harv", "Precent_harv"]) - (year0population[year0population$AgeClass == "Adult", "male"]*(1-((1- nhm_df[nhm_df$AgeClass == "admale_nhm", "monthlyRate"])^(seasonDuration)))))

  # Step 2
  N_adfemale_posth <- round((year0population[year0population$AgeClass == "Adult", "female"] - ((year0population[year0population$AgeClass == "Adult", "female"] - (year0population[year0population$AgeClass == "Adult", "female"]*(1 - ((1- nhm_df[nhm_df$AgeClass == "adfemale_nhm", "monthlyRate"])^(seasonDuration))))))*harv_df[harv_df$harvestClass == "adfemale_harv", "Precent_harv"]) - (year0population[year0population$AgeClass == "Adult", "female"]*(1-((1- nhm_df[nhm_df$AgeClass == "adfemale_nhm", "monthlyRate"])^(seasonDuration)))))

  # Step 3
  N_y1male_posth <- round((year0population[year0population$AgeClass == "Young", "male"] - ((year0population[year0population$AgeClass == "Young", "male"] - (year0population[year0population$AgeClass == "Young", "male"]*(1 - ((1- nhm_df[nhm_df$AgeClass == "admale_nhm", "monthlyRate"])^(seasonDuration))))))*harv_df[harv_df$harvestClass == "y1male_harv", "Precent_harv"]) - (year0population[year0population$AgeClass == "Young", "male"]*(1-((1- nhm_df[nhm_df$AgeClass == "admale_nhm", "monthlyRate"])^(seasonDuration)))))

  # Step 4
  N_y1female_posth <- round((year0population[year0population$AgeClass == "Young", "female"] - ((year0population[year0population$AgeClass == "Young", "female"] - (year0population[year0population$AgeClass == "Young", "female"]*(1 - ((1- nhm_df[nhm_df$AgeClass == "adfemale_nhm", "monthlyRate"])^(seasonDuration))))))*harv_df[harv_df$harvestClass == "y1female_harv", "Precent_harv"]) - (year0population[year0population$AgeClass == "Young", "female"]*(1-((1- nhm_df[nhm_df$AgeClass == "adfemale_nhm", "monthlyRate"])^(seasonDuration)))))

  # Step 5
  N_ymale_posth <- round((year0population[year0population$AgeClass == "Yearling", "male"] - ((year0population[year0population$AgeClass == "Yearling", "male"] - (year0population[year0population$AgeClass == "Yearling", "male"]*(1 - ((1- nhm_df[nhm_df$AgeClass == "yearmale_nhm", "monthlyRate"])^(seasonDuration))))))*harv_df[harv_df$harvestClass == "yearmale_harv", "Precent_harv"]) - (year0population[year0population$AgeClass == "Yearling", "male"]*(1-((1- nhm_df[nhm_df$AgeClass == "yearmale_nhm", "monthlyRate"])^(seasonDuration)))))

  # Step 6
  N_yfemale_posth <- round((year0population[year0population$AgeClass == "Yearling", "female"] - ((year0population[year0population$AgeClass == "Yearling", "female"] - (year0population[year0population$AgeClass == "Yearling", "female"]*(1 - ((1- nhm_df[nhm_df$AgeClass == "yearfemale_nhm", "monthlyRate"])^(seasonDuration))))))*harv_df[harv_df$harvestClass == "yearfemale_harv", "Precent_harv"]) - (year0population[year0population$AgeClass == "Yearling", "female"]*(1-((1- nhm_df[nhm_df$AgeClass == "yearfemale_nhm", "monthlyRate"])^(seasonDuration)))))

  # Step 7
  N_fawnmale_posth <- round((year0population[year0population$AgeClass == "Fawn", "male"] - ((year0population[year0population$AgeClass == "Fawn", "male"] - (year0population[year0population$AgeClass == "Fawn", "male"]*(1 - ((1- nhm_df[nhm_df$AgeClass == "fawnmale_nhm", "monthlyRate"])^(seasonDuration))))))*harv_df[harv_df$harvestClass == "fawnmale_harv", "Precent_harv"]) - (year0population[year0population$AgeClass == "Fawn", "male"]*(1-((1- nhm_df[nhm_df$AgeClass == "fawnmale_nhm", "monthlyRate"])^(seasonDuration)))))

  # Step 8
  N_fawnfemale_posth <-  round((year0population[year0population$AgeClass == "Fawn", "female"] - ((year0population[year0population$AgeClass == "Fawn", "female"] - (year0population[year0population$AgeClass == "Fawn", "female"]*(1 - ((1- nhm_df[nhm_df$AgeClass == "fawnfemale_nhm", "monthlyRate"])^(seasonDuration))))))*harv_df[harv_df$harvestClass == "fawnfemale_harv", "Precent_harv"]) - (year0population[year0population$AgeClass == "Fawn", "female"]*(1-((1- nhm_df[nhm_df$AgeClass == "fawnfemale_nhm", "monthlyRate"])^(seasonDuration)))))

  # Step 9
  N_postharvest <- N_admale_posth + N_adfemale_posth + N_y1male_posth + N_y1female_posth + N_ymale_posth + N_yfemale_posth + N_fawnmale_posth + N_fawnfemale_posth



  ### MidYear ###

  # Step 1
  N_admale_mid <- round((N_admale_posth + N_y1male_posth) - ((N_admale_posth + N_y1male_posth)* (1 - ((1- nhm_df[nhm_df$AgeClass == "admale_nhm", "monthlyRate"])^(12 - (seasonOpen +(seasonDuration - 1)) + birthMonth)))))

  # Step 2
  N_adfemale_mid <- round((N_adfemale_posth + N_y1female_posth) - ((N_adfemale_posth + N_y1female_posth)* (1 - ((1- nhm_df[nhm_df$AgeClass == "adfemale_nhm", "monthlyRate"])^(12 - (seasonOpen +(seasonDuration - 1)) + birthMonth)))))

  # Step 3
  N_y1male_mid <- round(N_ymale_posth - (N_ymale_posth * (1 - ((1- nhm_df[nhm_df$AgeClass == "yearmale_nhm", "monthlyRate"])^(12 - (seasonOpen +(seasonDuration - 1)) + birthMonth)))))

  # Step 4
  N_y1female_mid <- round(N_yfemale_posth - (N_yfemale_posth * (1 - ((1- nhm_df[nhm_df$AgeClass == "yearfemale_nhm", "monthlyRate"])^(12 - (seasonOpen +(seasonDuration - 1)) + birthMonth)))))

  # Step 5
  N_ymale_mid <- round(N_fawnmale_posth - (N_fawnmale_posth * (1 - ((1- nhm_df[nhm_df$AgeClass == "fawnmale_nhm", "monthlyRate"])^(12 - (seasonOpen +(seasonDuration - 1)) + birthMonth)))))

  # Step 6
  N_yfemale_mid <- round(N_fawnfemale_posth - (N_fawnfemale_posth * (1 - ((1- nhm_df[nhm_df$AgeClass == "fawnfemale_nhm", "monthlyRate"])^(12 - (seasonOpen +(seasonDuration - 1)) + birthMonth)))))

  # Step 7
  birth_malefawn <- round(((N_adfemale_mid*adFawnRate) + (N_y1female_mid*adFawnRate) + (N_yfemale_mid*yearFawnRate))/2)

  # Step 8
  birth_femalefawn <- round(((N_adfemale_mid*adFawnRate) + (N_y1female_mid*adFawnRate) + (N_yfemale_mid*yearFawnRate))/2)

  # Step 9
  N_midYear <- N_admale_mid +
    N_adfemale_mid +
    N_y1male_mid +
    N_y1female_mid +
    N_ymale_mid +
    N_yfemale_mid +
    birth_malefawn +
    birth_femalefawn


  ### Preharvest ###

  # Step 1
  N_admale_preh <- round(N_admale_mid - (N_admale_mid* (1-((1-nhm_df[nhm_df$AgeClass == "admale_nhm", "monthlyRate"])^((seasonOpen-1)- birthMonth)))))

  # Step 2
  N_adfemale_preh <- round(N_adfemale_mid - (N_adfemale_mid* (1-((1-nhm_df[nhm_df$AgeClass == "adfemale_nhm", "monthlyRate"])^((seasonOpen-1)- birthMonth)))))

  # Step 3
  N_y1male_preh <- round(N_y1male_mid - (N_y1male_mid* (1-((1-nhm_df[nhm_df$AgeClass == "admale_nhm", "monthlyRate"])^((seasonOpen-1)- birthMonth)))))

  # Step 4
  N_y1female_preh <- round(N_y1female_mid - (N_y1female_mid* (1-((1-nhm_df[nhm_df$AgeClass == "adfemale_nhm", "monthlyRate"])^((seasonOpen-1)- birthMonth)))))

  # Step 5
  N_ymale_preh <- round(N_ymale_mid - (N_ymale_mid* (1-((1-nhm_df[nhm_df$AgeClass == "yearmale_nhm", "monthlyRate"])^((seasonOpen-1)- birthMonth)))))

  # Step 6
  N_yfemale_preh <- round(N_yfemale_mid - (N_yfemale_mid* (1-((1-nhm_df[nhm_df$AgeClass == "yearfemale_nhm", "monthlyRate"])^((seasonOpen-1)- birthMonth)))))

  # Step 7
  N_fawnmale_preh <- round(birth_malefawn - (birth_malefawn* (1-((1-nhm_df[nhm_df$AgeClass == "sixmonthfawn_nhm", "monthlyRate"])^((seasonOpen-1)- birthMonth)))))

  # Step 8
  N_fawnfemale_preh <- round(birth_femalefawn - (birth_femalefawn* (1-((1-nhm_df[nhm_df$AgeClass == "sixmonthfawn_nhm", "monthlyRate"])^((seasonOpen-1)- birthMonth)))))

  # Step 9
  N_preharvest <-  N_admale_preh +
    N_adfemale_preh +
    N_y1male_preh +
    N_y1female_preh +
    N_ymale_preh +
    N_yfemale_preh +
    N_fawnmale_preh +
    N_fawnfemale_preh

  ### Harvest ###

  # Step 1
  N_admale_harvest <- round(N_admale_preh*harv_df[harv_df$harvestClass == "admale_harv", "Precent_harv"])

  # Step 2
  N_adfemale_harvest <- round(N_adfemale_preh*harv_df[harv_df$harvestClass == "adfemale_harv", "Precent_harv"])

  # Step 3
  N_y1male_harvest <- round(N_y1male_preh*harv_df[harv_df$harvestClass == "y1male_harv", "Precent_harv"])

  # Step 4
  N_y1female_harvest <- round(N_y1female_preh*harv_df[harv_df$harvestClass == "y1female_harv", "Precent_harv"])

  # Step 5
  N_ymale_harvest <- round(N_ymale_preh*harv_df[harv_df$harvestClass == "yearmale_harv", "Precent_harv"])

  # Step 6
  N_yfemale_harvest <- round(N_yfemale_preh*harv_df[harv_df$harvestClass == "yearfemale_harv", "Precent_harv"])

  # Step 7
  N_fawnmale_harvest <- round(N_fawnmale_preh*harv_df[harv_df$harvestClass == "fawnmale_harv", "Precent_harv"])

  # Step 8
  N_fawnfemale_harvest <- round(N_fawnfemale_preh*harv_df[harv_df$harvestClass == "fawnfemale_harv", "Precent_harv"])

  # Step 9
  N_harvest <-  N_admale_harvest +
    N_adfemale_harvest +
    N_y1male_harvest +
    N_y1female_harvest +
    N_ymale_harvest +
    N_yfemale_harvest +
    N_fawnmale_harvest +
    N_fawnfemale_harvest

  # Combine all results into one data frame
  previous_year <- data.frame(
    N_admale_posth,
    N_adfemale_posth,
    N_y1male_posth,
    N_y1female_posth,
    N_ymale_posth,
    N_yfemale_posth,
    N_fawnmale_posth,
    N_fawnfemale_posth,
    N_postharvest,
    N_admale_mid,
    N_adfemale_mid,
    N_y1male_mid,
    N_y1female_mid,
    N_ymale_mid,
    N_yfemale_mid,
    birth_malefawn,
    birth_femalefawn,
    N_midYear,
    N_admale_preh,
    N_adfemale_preh,
    N_y1male_preh,
    N_y1female_preh,
    N_ymale_preh,
    N_yfemale_preh,
    N_fawnmale_preh,
    N_fawnfemale_preh,
    N_preharvest,
    N_admale_harvest,
    N_adfemale_harvest,
    N_y1male_harvest,
    N_y1female_harvest,
    N_ymale_harvest,
    N_yfemale_harvest,
    N_fawnmale_harvest,
    N_fawnfemale_harvest,
    N_harvest
  )

  ########### Beyond year 1 ###############
  year_fun <- function(
    previous_year,
    seasonDuration,
    seasonOpen,
    birthMonth,
    adFawnRate,
    yearFawnRate,
    harvest_chng_admale,
    harvest_chng_adfemale,
    harvest_chng_y1male,
    harvest_chng_y1female,
    harvest_chng_ymale,
    harvest_chng_yfemale,
    harvest_chng_fawnmale,
    harvest_chng_fawnfemale,
    adMort_admale,
    adMort_adfemale,
    adMort_y1male,
    adMort_y1female,
    adMort_ymale,
    adMort_yfemale,
    adMort_fawnmale,
    adMort_fawnfemale
  ) {

    ### Postharvest ###

    N_admale_posth <- round(
      previous_year$N_admale_preh -
        (
          previous_year$N_admale_preh -
            (
              previous_year$N_admale_preh *
                (1 - ((1 - nhm_df[nhm_df$AgeClass == "admale_nhm",
                                  "monthlyRate"])^seasonDuration))
            )
        ) *
        (
          harv_df[harv_df$harvestClass == "admale_harv",
                  "Precent_harv"] *
            (1 + harvest_chng_admale)
        ) -
        (
          previous_year$N_admale_preh *
            (1 - ((1 - nhm_df[nhm_df$AgeClass == "admale_nhm",
                              "monthlyRate"])^seasonDuration))
        )
    )

    N_adfemale_posth <- round(
      previous_year$N_adfemale_preh -
        (
          previous_year$N_adfemale_preh -
            (
              previous_year$N_adfemale_preh *
                (1 - ((1 - nhm_df[nhm_df$AgeClass == "adfemale_nhm",
                                  "monthlyRate"])^seasonDuration))
            )
        ) *
        (
          harv_df[harv_df$harvestClass == "adfemale_harv",
                  "Precent_harv"] *
            (1 + harvest_chng_adfemale)
        ) -
        (
          previous_year$N_adfemale_preh *
            (1 - ((1 - nhm_df[nhm_df$AgeClass == "adfemale_nhm",
                              "monthlyRate"])^seasonDuration))
        )
    )

    N_y1male_posth <- round(
      previous_year$N_y1male_preh -
        (
          previous_year$N_y1male_preh -
            (
              previous_year$N_y1male_preh *
                (1 - ((1 - nhm_df[nhm_df$AgeClass == "admale_nhm",
                                  "monthlyRate"])^seasonDuration))
            )
        ) *
        (
          harv_df[harv_df$harvestClass == "y1male_harv",
                  "Precent_harv"] *
            (1 + harvest_chng_y1male)
        ) -
        (
          previous_year$N_y1male_preh *
            (1 - ((1 - nhm_df[nhm_df$AgeClass == "admale_nhm",
                              "monthlyRate"])^seasonDuration))
        )
    )

    N_y1female_posth <- round(
      previous_year$N_y1female_preh -
        (
          previous_year$N_y1female_preh -
            (
              previous_year$N_y1female_preh *
                (1 - ((1 - nhm_df[nhm_df$AgeClass == "adfemale_nhm",
                                  "monthlyRate"])^seasonDuration))
            )
        ) *
        (
          harv_df[harv_df$harvestClass == "y1female_harv",
                  "Precent_harv"] *
            (1 + harvest_chng_y1female)
        ) -
        (
          previous_year$N_y1female_preh *
            (1 - ((1 - nhm_df[nhm_df$AgeClass == "adfemale_nhm",
                              "monthlyRate"])^seasonDuration))
        )
    )

    N_ymale_posth <- round(
      previous_year$N_ymale_preh -
        (
          previous_year$N_ymale_preh -
            (
              previous_year$N_ymale_preh *
                (1 - ((1 - nhm_df[nhm_df$AgeClass == "yearmale_nhm",
                                  "monthlyRate"])^seasonDuration))
            )
        ) *
        (
          harv_df[harv_df$harvestClass == "yearmale_harv",
                  "Precent_harv"] *
            (1 + harvest_chng_ymale)
        ) -
        (
          previous_year$N_ymale_preh *
            (1 - ((1 - nhm_df[nhm_df$AgeClass == "yearmale_nhm",
                              "monthlyRate"])^seasonDuration))
        )
    )

    N_yfemale_posth <- round(
      previous_year$N_yfemale_preh -
        (
          previous_year$N_yfemale_preh -
            (
              previous_year$N_yfemale_preh *
                (1 - ((1 - nhm_df[nhm_df$AgeClass == "yearfemale_nhm",
                                  "monthlyRate"])^seasonDuration))
            )
        ) *
        (
          harv_df[harv_df$harvestClass == "yearfemale_harv",
                  "Precent_harv"] *
            (1 + harvest_chng_yfemale)
        ) -
        (
          previous_year$N_yfemale_preh *
            (1 - ((1 - nhm_df[nhm_df$AgeClass == "yearfemale_nhm",
                              "monthlyRate"])^seasonDuration))
        )
    )

    N_fawnmale_posth <- round(
      previous_year$N_fawnmale_preh -
        (
          previous_year$N_fawnmale_preh -
            (
              previous_year$N_fawnmale_preh *
                (1 - ((1 - nhm_df[nhm_df$AgeClass == "fawnmale_nhm",
                                  "monthlyRate"])^seasonDuration))
            )
        ) *
        (
          harv_df[harv_df$harvestClass == "fawnmale_harv",
                  "Precent_harv"] *
            (1 + harvest_chng_fawnmale)
        ) -
        (
          previous_year$N_fawnmale_preh *
            (1 - ((1 - nhm_df[nhm_df$AgeClass == "fawnmale_nhm",
                              "monthlyRate"])^seasonDuration))
        )
    )

    N_fawnfemale_posth <- round(
      previous_year$N_fawnfemale_preh -
        (
          previous_year$N_fawnfemale_preh -
            (
              previous_year$N_fawnfemale_preh *
                (1 - ((1 - nhm_df[nhm_df$AgeClass == "fawnfemale_nhm",
                                  "monthlyRate"])^seasonDuration))
            )
        ) *
        (
          harv_df[harv_df$harvestClass == "fawnfemale_harv",
                  "Precent_harv"] *
            (1 + harvest_chng_fawnfemale)
        ) -
        (
          previous_year$N_fawnfemale_preh *
            (1 - ((1 - nhm_df[nhm_df$AgeClass == "fawnfemale_nhm",
                              "monthlyRate"])^seasonDuration))
        )
    )

    N_postharvest <-
      N_admale_posth +
      N_adfemale_posth +
      N_y1male_posth +
      N_y1female_posth +
      N_ymale_posth +
      N_yfemale_posth +
      N_fawnmale_posth +
      N_fawnfemale_posth


    ### MidYear ###

    N_admale_mid <- round(
      (N_admale_posth + N_y1male_posth) -
        ((N_admale_posth + N_y1male_posth) *
           (1 - ((1 - nhm_df[nhm_df$AgeClass == "admale_nhm",
                             "monthlyRate"])^(
                               12 - (seasonOpen + (seasonDuration - 1)) +
                                 birthMonth))))
    )

    N_adfemale_mid <- round(
      (N_adfemale_posth + N_y1female_posth) -
        ((N_adfemale_posth + N_y1female_posth) *
           (1 - ((1 - nhm_df[nhm_df$AgeClass == "adfemale_nhm",
                             "monthlyRate"])^(
                               12 - (seasonOpen + (seasonDuration - 1)) +
                                 birthMonth))))
    )

    N_y1male_mid <- round(
      N_ymale_posth -
        (N_ymale_posth *
           (1 - ((1 - nhm_df[nhm_df$AgeClass == "yearmale_nhm",
                             "monthlyRate"])^(
                               12 - (seasonOpen + (seasonDuration - 1)) +
                                 birthMonth))))
    )

    N_y1female_mid <- round(
      N_yfemale_posth -
        (N_yfemale_posth *
           (1 - ((1 - nhm_df[nhm_df$AgeClass == "yearfemale_nhm",
                             "monthlyRate"])^(
                               12 - (seasonOpen + (seasonDuration - 1)) +
                                 birthMonth))))
    )

    N_ymale_mid <- round(
      N_fawnmale_posth -
        (N_fawnmale_posth *
           (1 - ((1 - nhm_df[nhm_df$AgeClass == "fawnmale_nhm",
                             "monthlyRate"])^(
                               12 - (seasonOpen + (seasonDuration - 1)) +
                                 birthMonth))))
    )

    N_yfemale_mid <- round(
      N_fawnfemale_posth -
        (N_fawnfemale_posth *
           (1 - ((1 - nhm_df[nhm_df$AgeClass == "fawnfemale_nhm",
                             "monthlyRate"])^(
                               12 - (seasonOpen + (seasonDuration - 1)) +
                                 birthMonth))))
    )

    birth_malefawn <- round(
      (
        (N_adfemale_mid * adFawnRate) +
          (N_y1female_mid * adFawnRate) +
          (N_yfemale_mid * yearFawnRate)
      ) / 2
    )

    birth_femalefawn <- round(
      (
        (N_adfemale_mid * adFawnRate) +
          (N_y1female_mid * adFawnRate) +
          (N_yfemale_mid * yearFawnRate)
      ) / 2
    )

    N_midYear <-
      N_admale_mid +
      N_adfemale_mid +
      N_y1male_mid +
      N_y1female_mid +
      N_ymale_mid +
      N_yfemale_mid +
      birth_malefawn +
      birth_femalefawn


    ### Preharvest ###

    N_admale_preh <- round(
      (N_admale_mid -
         N_admale_mid *
         (1 - ((1 - nhm_df[nhm_df$AgeClass == "admale_nhm",
                           "monthlyRate"])^((seasonOpen - 1) - birthMonth)))) -
        (
          (
            N_admale_mid -
              N_admale_mid *
              (1 - ((1 - nhm_df[nhm_df$AgeClass == "admale_nhm",
                                "monthlyRate"])^((seasonOpen - 1) -
                                                   birthMonth)))
          ) * adMort_admale
        )
    )

    N_adfemale_preh <- round(
      (N_adfemale_mid -
         N_adfemale_mid *
         (1 - ((1 - nhm_df[nhm_df$AgeClass == "adfemale_nhm",
                           "monthlyRate"])^((seasonOpen - 1) - birthMonth)))) -
        (
          (
            N_adfemale_mid -
              N_adfemale_mid *
              (1 - ((1 - nhm_df[nhm_df$AgeClass == "adfemale_nhm",
                                "monthlyRate"])^((seasonOpen - 1) -
                                                   birthMonth)))
          ) * adMort_adfemale
        )
    )

    N_y1male_preh <- round(
      (N_y1male_mid -
         N_y1male_mid *
         (1 - ((1 - nhm_df[nhm_df$AgeClass == "admale_nhm",
                           "monthlyRate"])^((seasonOpen - 1) - birthMonth)))) -
        (
          (
            N_y1male_mid -
              N_y1male_mid *
              (1 - ((1 - nhm_df[nhm_df$AgeClass == "admale_nhm",
                                "monthlyRate"])^((seasonOpen - 1) -
                                                   birthMonth)))
          ) * adMort_y1male
        )
    )

    N_y1female_preh <- round(
      (N_y1female_mid -
         N_y1female_mid *
         (1 - ((1 - nhm_df[nhm_df$AgeClass == "adfemale_nhm",
                           "monthlyRate"])^((seasonOpen - 1) - birthMonth)))) -
        (
          (
            N_y1female_mid -
              N_y1female_mid *
              (1 - ((1 - nhm_df[nhm_df$AgeClass == "adfemale_nhm",
                                "monthlyRate"])^((seasonOpen - 1) -
                                                   birthMonth)))
          ) * adMort_y1female
        )
    )

    N_ymale_preh <- round(
      (N_ymale_mid -
         N_ymale_mid *
         (1 - ((1 - nhm_df[nhm_df$AgeClass == "yearmale_nhm",
                           "monthlyRate"])^((seasonOpen - 1) - birthMonth)))) -
        (
          (
            N_ymale_mid -
              N_ymale_mid *
              (1 - ((1 - nhm_df[nhm_df$AgeClass == "yearmale_nhm",
                                "monthlyRate"])^((seasonOpen - 1) -
                                                   birthMonth)))
          ) * adMort_ymale
        )
    )

    N_yfemale_preh <- round(
      (N_yfemale_mid -
         N_yfemale_mid *
         (1 - ((1 - nhm_df[nhm_df$AgeClass == "yearfemale_nhm",
                           "monthlyRate"])^((seasonOpen - 1) - birthMonth)))) -
        (
          (
            N_yfemale_mid -
              N_yfemale_mid *
              (1 - ((1 - nhm_df[nhm_df$AgeClass == "yearfemale_nhm",
                                "monthlyRate"])^((seasonOpen - 1) -
                                                   birthMonth)))
          ) * adMort_yfemale
        )
    )

    N_fawnmale_preh <- round(
      (birth_malefawn -
         birth_malefawn *
         (1 - ((1 - nhm_df[nhm_df$AgeClass == "sixmonthfawn_nhm",
                           "monthlyRate"])^((seasonOpen - 1) - birthMonth)))) -
        (
          (
            birth_malefawn -
              birth_malefawn *
              (1 - ((1 - nhm_df[nhm_df$AgeClass == "sixmonthfawn_nhm",
                                "monthlyRate"])^((seasonOpen - 1) -
                                                   birthMonth)))
          ) * adMort_fawnmale
        )
    )

    N_fawnfemale_preh <- round(
      (birth_femalefawn -
         birth_femalefawn *
         (1 - ((1 - nhm_df[nhm_df$AgeClass == "sixmonthfawn_nhm",
                           "monthlyRate"])^((seasonOpen - 1) - birthMonth)))) -
        (
          (
            birth_femalefawn -
              birth_femalefawn *
              (1 - ((1 - nhm_df[nhm_df$AgeClass == "sixmonthfawn_nhm",
                                "monthlyRate"])^((seasonOpen - 1) -
                                                   birthMonth)))
          ) * adMort_fawnfemale
        )
    )

    N_preharvest <-
      N_admale_preh +
      N_adfemale_preh +
      N_y1male_preh +
      N_y1female_preh +
      N_ymale_preh +
      N_yfemale_preh +
      N_fawnmale_preh +
      N_fawnfemale_preh


    ### Harvest ###

    N_admale_harvest <- round(
      N_admale_preh *
        (
          harv_df[harv_df$harvestClass == "admale_harv",
                  "Precent_harv"] *
            (1 + harvest_chng_admale)
        )
    )

    N_adfemale_harvest <- round(
      N_adfemale_preh *
        (
          harv_df[harv_df$harvestClass == "adfemale_harv",
                  "Precent_harv"] *
            (1 + harvest_chng_adfemale)
        )
    )

    N_y1male_harvest <- round(
      N_y1male_preh *
        (
          harv_df[harv_df$harvestClass == "y1male_harv",
                  "Precent_harv"] *
            (1 + harvest_chng_y1male)
        )
    )

    N_y1female_harvest <- round(
      N_y1female_preh *
        (
          harv_df[harv_df$harvestClass == "y1female_harv",
                  "Precent_harv"] *
            (1 + harvest_chng_y1female)
        )
    )

    N_ymale_harvest <- round(
      N_ymale_preh *
        (
          harv_df[harv_df$harvestClass == "yearmale_harv",
                  "Precent_harv"] *
            (1 + harvest_chng_ymale)
        )
    )

    N_yfemale_harvest <- round(
      N_yfemale_preh *
        (
          harv_df[harv_df$harvestClass == "yearfemale_harv",
                  "Precent_harv"] *
            (1 + harvest_chng_yfemale)
        )
    )

    N_fawnmale_harvest <- round(
      N_fawnmale_preh *
        (
          harv_df[harv_df$harvestClass == "fawnmale_harv",
                  "Precent_harv"] *
            (1 + harvest_chng_fawnmale)
        )
    )

    N_fawnfemale_harvest <- round(
      N_fawnfemale_preh *
        (
          harv_df[harv_df$harvestClass == "fawnfemale_harv",
                  "Precent_harv"] *
            (1 + harvest_chng_fawnfemale)
        )
    )

    N_harvest <-
      N_admale_harvest +
      N_adfemale_harvest +
      N_y1male_harvest +
      N_y1female_harvest +
      N_ymale_harvest +
      N_yfemale_harvest +
      N_fawnmale_harvest +
      N_fawnfemale_harvest


    ### Return one year ###

    data.frame(
      N_admale_posth = N_admale_posth,
      N_adfemale_posth = N_adfemale_posth,
      N_y1male_posth = N_y1male_posth,
      N_y1female_posth = N_y1female_posth,
      N_ymale_posth = N_ymale_posth,
      N_yfemale_posth = N_yfemale_posth,
      N_fawnmale_posth = N_fawnmale_posth,
      N_fawnfemale_posth = N_fawnfemale_posth,
      N_postharvest = N_postharvest,
      N_admale_mid = N_admale_mid,
      N_adfemale_mid = N_adfemale_mid,
      N_y1male_mid = N_y1male_mid,
      N_y1female_mid = N_y1female_mid,
      N_ymale_mid = N_ymale_mid,
      N_yfemale_mid = N_yfemale_mid,
      birth_malefawn = birth_malefawn,
      birth_femalefawn = birth_femalefawn,
      N_midYear = N_midYear,
      N_admale_preh = N_admale_preh,
      N_adfemale_preh = N_adfemale_preh,
      N_y1male_preh = N_y1male_preh,
      N_y1female_preh = N_y1female_preh,
      N_ymale_preh = N_ymale_preh,
      N_yfemale_preh = N_yfemale_preh,
      N_fawnmale_preh = N_fawnmale_preh,
      N_fawnfemale_preh = N_fawnfemale_preh,
      N_preharvest = N_preharvest,
      N_admale_harvest = N_admale_harvest,
      N_adfemale_harvest = N_adfemale_harvest,
      N_y1male_harvest = N_y1male_harvest,
      N_y1female_harvest = N_y1female_harvest,
      N_ymale_harvest = N_ymale_harvest,
      N_yfemale_harvest = N_yfemale_harvest,
      N_fawnmale_harvest = N_fawnmale_harvest,
      N_fawnfemale_harvest = N_fawnfemale_harvest,
      N_harvest = N_harvest
    )
  }


  ############# Loop #################

  # Store all 15 years
  pop_results2 <- vector("list", 15)

  # Year 1 is already calculated above
  pop_results2[[1]] <- previous_year


  # Years 2 through 15
  for (i in 2:15) {

    if (!iterative) {

      harvest_active <- i >= 4 &&
        i <= (3 + changeDuration)

    } else {

      harvest_active <- i >= 4 &&
        i <= (3 + changeDuration * 4) &&
        ((i - 4) %% 4 < 2)
    }

    if (harvest_active) {

      harvest_chng_admale2     <- harvest_chng_admale
      harvest_chng_adfemale2   <- harvest_chng_adfemale
      harvest_chng_y1male2     <- harvest_chng_y1male
      harvest_chng_y1female2   <- harvest_chng_y1female
      harvest_chng_ymale2      <- harvest_chng_ymale
      harvest_chng_yfemale2    <- harvest_chng_yfemale
      harvest_chng_fawnmale2   <- harvest_chng_fawnmale
      harvest_chng_fawnfemale2 <- harvest_chng_fawnfemale

    } else {

      harvest_chng_admale2     <- 0
      harvest_chng_adfemale2   <- 0
      harvest_chng_y1male2     <- 0
      harvest_chng_y1female2   <- 0
      harvest_chng_ymale2      <- 0
      harvest_chng_yfemale2    <- 0
      harvest_chng_fawnmale2   <- 0
      harvest_chng_fawnfemale2 <- 0
    }

    #Additional mortality change

    if (i >= 4 && i <= (3 + adMortChangeDuration)) {

      adMort_admale2     <- adMort_admale
      adMort_adfemale2   <- adMort_adfemale
      adMort_y1male2     <- adMort_y1male
      adMort_y1female2   <- adMort_y1female
      adMort_ymale2      <- adMort_ymale
      adMort_yfemale2    <- adMort_yfemale
      adMort_fawnmale2   <- adMort_fawnmale
      adMort_fawnfemale2 <- adMort_fawnfemale

    } else {

      adMort_admale2     <- 0
      adMort_adfemale2   <- 0
      adMort_y1male2     <- 0
      adMort_y1female2   <- 0
      adMort_ymale2      <- 0
      adMort_yfemale2    <- 0
      adMort_fawnmale2   <- 0
      adMort_fawnfemale2 <- 0
    }


    #Running next year

    pop_results2[[i]] <- year_fun(

      previous_year = pop_results2[[i - 1]],

      seasonDuration = seasonDuration,
      seasonOpen = seasonOpen,
      birthMonth = birthMonth,
      adFawnRate = adFawnRate,
      yearFawnRate = yearFawnRate,

      harvest_chng_admale = harvest_chng_admale2,
      harvest_chng_adfemale = harvest_chng_adfemale2,
      harvest_chng_y1male = harvest_chng_y1male2,
      harvest_chng_y1female = harvest_chng_y1female2,
      harvest_chng_ymale = harvest_chng_ymale2,
      harvest_chng_yfemale = harvest_chng_yfemale2,
      harvest_chng_fawnmale = harvest_chng_fawnmale2,
      harvest_chng_fawnfemale = harvest_chng_fawnfemale2,

      adMort_admale = adMort_admale2,
      adMort_adfemale = adMort_adfemale2,
      adMort_y1male = adMort_y1male2,
      adMort_y1female = adMort_y1female2,
      adMort_ymale = adMort_ymale2,
      adMort_yfemale = adMort_yfemale2,
      adMort_fawnmale = adMort_fawnmale2,
      adMort_fawnfemale = adMort_fawnfemale2
    )
  }

  population_results2 <- do.call(
    rbind,
    pop_results2
  )

  # Reset row names
  rownames(population_results2) <- NULL


  # Add model year
  population_results2$ModelYear <- 1:15

  # Add Lambda
  population_results2$Lambda <- NA_real_

  population_results2$Lambda[2:15] <-
    population_results2$N_preharvest[2:15] /
    population_results2$N_preharvest[1:14]

  population_results2$Lambda <- round(population_results2$Lambda, digits = 2)

  return(population_results2)

}
