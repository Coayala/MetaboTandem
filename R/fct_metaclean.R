#' metaclean
#'
#' @description A fct function
#'
#' @importFrom magrittr `%<>%`
#'
#' @return The return value, if any, from executing the function.
#'
apply_cleaning <- function(data,
                           group_by,
                           model_file) {

  # Transforming into an xcms object
  xcms_set <- as(xcms::filterMsLevel(data, msLevel = 1), 'xcmsSet')
  xcms::sampclass(xcms_set) <- data@sampleData[[group_by]]

  # xcms gap filling
  xcms_fill <- xcms::fillPeaks(xcms_set)

  # Checking peaks present in xcmsSet and MsExperiment objects
  ft_def <- xcms::featureDefinitions(data)
  peak_group_names_available <- data.frame(
    FeatureID = rownames(ft_def),
    group_name = xcms::groupnames(xcms_set)
  ) %>%
    dplyr::filter(group_name %in% xcms::groupnames(xcms_fill))

  # Applying metaclean
  available_peaks <- stringr::str_remove(peak_group_names_available$FeatureID, 'FT') %>%
    as.numeric

  all_eic <- xcms::getEIC(xcms_set,
                          groupidx = available_peaks,
                          rt = 'corrected')

  all_eval <- MetaClean::getEvalObj(xs = all_eic,
                                    fill = xcms_fill)

  fill_group <- xcms::groupnames(xcms_fill)

  all_peak_quality <- MetaClean::getPeakQualityMetrics(eicEvalData = all_eval)

  fill_gnames <- fill_group[all_peak_quality$EICNo]

  all_peak_quality_filt <- all_peak_quality %>%
    dplyr::mutate(group_name = fill_gnames) %>%
    dplyr::left_join(peak_group_names_available) %>%
    tidyr::drop_na()

  metaclean_model <- readr::read_rds(model_file)

  all_predictions <- MetaClean::getPredicitons(model = metaclean_model,
                                               testData = all_peak_quality_filt,
                                               eicColumn = 'FeatureID')

  pass_metabolites <- all_predictions %>%
    dplyr::filter(Pred_Class == 'Pass') %>%
    dplyr::mutate(FeatureID = EIC)

  data_filt <- xcms::filterFeatureDefinitions(data,
                                              features = pass_metabolites$FeatureID)

  return(data_filt)
}


#' Prepare data for training a metaclean model
#'
#' @description Function to prepare dataset for training a metaclean model
#'
#' @importFrom magrittr `%<>%`
#'
#' @return The return value, if any, from executing the function.
#'
#' @noRd
prepare_for_cleaning <- function(data, metadata, feature_definitions, n_subset){

  xcms_set <- as(xcms::filterMsLevel(data, msLevel = 1), 'xcmsSet')
  xcms::sampclass(xcms_set) <- metadata[[2]]

  xcms_fill <- xcms::fillPeaks(xcms_set)

  peak_group_names_available <- data.frame(
    FeatureID = feature_definitions$feature_id,
    group_name = xcms::groupnames(xcms_set)
  ) %>%
    dplyr::filter(group_name %in% xcms::groupnames(xcms_fill))
  # Select peaks and extract eics

  sel_peaks <- sample(peak_group_names_available$FeatureID, n_subset) %>%
    stringr::str_remove('FT') %>%
    as.numeric

  chroms <- xcms::featureChromatograms(data,
                                       features = feature_definitions$feature_id[sel_peaks],
                                       filled = TRUE)
  # keep <- logical(length(sel_peaks))
  # for(i in seq_along(sel_peaks)){
  #   keep[i] <- !MSnbase::isEmpty(chroms[i,])
  # }

  res <- list(sel_peaks = sel_peaks,
              xcms_set = xcms_set,
              xcms_fill = xcms_fill,
              chroms = chroms)
}

#' Asses_peaks
#'
#' @description Function to prepare dataset for training a metaclean model
#'
#' @importFrom magrittr `%<>%`
#'
#' @return The return value, if any, from executing the function.
#'
assess_peaks <- function(prepare_obj, feature_definitions, perc_test){

  labels <- data.frame(
    feature_id = feature_definitions$feature_id[prepare_obj$sel_peaks]
  ) %>%
    dplyr::mutate(Label = NA)

  for(i in seq_along(prepare_obj$sel_peaks)){
    ft_id <-  labels$feature_id[i]
    xcms::plot(prepare_obj$chroms[i,],
               main = ft_id)

    labels$Label[i] <- readline(
      prompt = paste0(i, '. Enter P (Pass) or F (Fail): ')
    )
    while(!(labels$Label[i] %in% c('P', 'F'))){
      labels$Label[i] <- readline(
        prompt = 'ERROR: Re-Enter P (Pass) or F (Fail): ')
    }
  }

  labels <- labels %>%
    dplyr::mutate(Label = ifelse(Label == 'P', 'Pass', 'Fail'),
                  EICNo = stringr::str_remove(feature_id, 'FT'),
                  EICNo = as.numeric(EICNo))

  labels_train <- labels[sample(1:nrow(labels), nrow(labels) * perc_test),]
  labels_test <- labels %>%
    dplyr::filter(!feature_id %in% labels_train$feature_id)

  return(list(labels_train = labels_train,
              labels_test = labels_test))
}

#' Calculate models
#'
#' @description Function to prepare dataset for training a metaclean model
#'
#' @importFrom magrittr `%<>%`
#'
#' @return The return value, if any, from executing the function.
#'
get_models <- function(prepare_obj,
                       labels,
                       model_list = c('LogisticRegression',
                                      'NaiveBayes',
                                      'RandomForest',
                                      'SVM_Linear',
                                      'AdaBoost',
                                      'NeuralNetwork',
                                      'ModelAveragedNeuralNetwork'),
                       metric_list = c('M4', 'M7', 'M11')){

  eic_dev <- xcms::getEIC(prepare_obj$xcms_set,
                          groupidx = labels$labels_train$EICNo,
                          rt = 'corrected')

  eic_eval_dev <- MetaClean::getEvalObj(xs = eic_dev,
                                        fill = prepare_obj$xcms_fill)

  peak_qual_dev <- MetaClean::getPeakQualityMetrics(
    eicEvalData = eic_eval_dev,
    eicLabels_df = as.data.frame(labels$labels_train)
  ) %>%
    tidyr::drop_na()

  models <- list()

  for(mod in model_list){

    for(metric in metric_list){
      model_id <- paste0(mod, '_', metric)

      print(model_id)

      cross_v <- try(
        MetaClean::runCrossValidation(trainData = peak_qual_dev,
                                      k = 6,
                                      repNum = 10,
                                      rand.seed = 512,
                                      models = mod,
                                      metricSet = metric)
      )

      if(!inherits(cross_v, "try-error")){
        models[model_id] <- cross_v
      } else {
        models[model_id] <- NULL
      }

    }
  }

  models <- purrr::map(models, function(mod){

    mod$pred <- mod$pred %>%
      tidyr::drop_na()

    return(mod)

  })

  return(list(models = models,
              peak_qual_met = peak_qual_dev))
}

#' Calculate models
#'
#' @description Function to prepare dataset for training a metaclean model
#'
#' @importFrom magrittr `%<>%`
#'
#' @return The return value, if any, from executing the function.
#'
evaluate_models <- function(models){

  eval_measures <- MetaClean::getEvaluationMeasures(models=models$models, k=5, repNum=10)

  bar_plots <- MetaClean::getBarPlots(eval_measures, emNames="All")

  cd_plots <- MetaClean::getCDPlots(evalMeasuresDF = eval_measures,
                                    emNames = c("Pass_FScore", "Fail_FScore", "Accuracy"),
                                    compareBest = T,
                                    use_abbr = F)

  return(list(eval_measures = eval_measures,
              bar_plots = bar_plots,
              cd_plots = cd_plots))

}

#' Calculate final model
#'
#' @description Function to prepare dataset for training a metaclean model
#'
#' @importFrom magrittr `%<>%`
#'
#' @return The return value, if any, from executing the function.
#'
train_final_model <- function(models, selected_model){

  hyperparameters <- models$models[[selected_model]]$pred %>%
    dplyr::select(-c(pred, obs, Fail, Pass, rowIndex, Resample)) %>%
    dplyr::distinct()

  mod_metric <- strsplit(selected_model, '_') %>% unlist()

  if(length(mod_metric) > 2){
    temp <- c()
    temp[1] <- paste0(mod_metric[1], '_', mod_metric[2])
    temp[2] <- mod_metric[3]

    mod_metric <- temp
  }

  metaclean_model <- MetaClean::trainClassifier(trainData = models$peak_qual_met,
                                                model = mod_metric[1],
                                                metric = mod_metric[2],
                                                hyperparameters = hyperparameters)

  return(metaclean_model)
}

#' Calculate final model
#'
#' @description Function to prepare dataset for training a metaclean model
#'
#' @importFrom magrittr `%<>%`
#'
#' @return The return value, if any, from executing the function.
#'
test_final_model <- function(final_model, prepare_obj, labels){

  eic_test <- xcms::getEIC(prepare_obj$xcms_set,
                           groupidx = labels$labels_test$EICNo,
                           rt = 'corrected')

  eic_eval_test <- MetaClean::getEvalObj(xs = eic_test,
                                         fill = prepare_obj$xcms_fill)

  peak_qual_test <- MetaClean::getPeakQualityMetrics(
    eicEvalData = eic_eval_test,
    eicLabels_df = as.data.frame(labels$labels_test)
  ) %>%
    tidyr::drop_na()

  predictions <- MetaClean::getPredicitons(final_model,
                                           testData = peak_qual_test,
                                           eicColumn = 'EICNo') %>%
    dplyr::left_join(labels$labels_test, by = c('EIC' = 'EICNo'))


  predictions_coded <- predictions %>%
    dplyr::select(Pred_Class, Label) %>%
    dplyr::mutate(Pred_Class = as.numeric(Pred_Class),
                  Label = factor(Label, levels = c('Fail', 'Pass')),
                  Label = as.numeric(Label))

  eval_metrics <- data.frame(
    Precision = MLmetrics::Precision(y_true = predictions_coded$Label,
                                     y_pred = predictions_coded$Pred_Class,
                                     positive = 2),
    Accuracy = MLmetrics::Accuracy(y_true = predictions_coded$Label,
                                   y_pred = predictions_coded$Pred_Class),
    Recall = MLmetrics::Recall(y_true = predictions_coded$Label,
                               y_pred = predictions_coded$Pred_Class,
                               positive = 2),
    FScore = MLmetrics::F1_Score(y_true = predictions_coded$Label,
                                 y_pred = predictions_coded$Pred_Class,
                                 positive = 2)
  )

  return(list(predictions = predictions,
              eval_metrics = eval_metrics))
}
