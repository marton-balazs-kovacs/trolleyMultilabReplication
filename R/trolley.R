#' Codebook for the processed datafile
#'
#' This datafile contains the codebook for the processed data of
#' the PSA006 Trolley multilab replication.
#'
#' @format A dataframe with 27502 rows and 152 variables:
#' \describe{
#'   \item{survey_name}{character, Either "PSA006_Eastern", "PSA006_Southern", or "PSA006_Western". The distinction is only important because of the materials.}
#'   \item{survey_id}{character, Unique id corresponding to the survey_name.}
#'   \item{StartDate}{date, Datetime at the start of the completition of the survey.}
#'   \item{EndDate}{date, Datetime at the end of the completition of the survey.}
#'   \item{ResponseId}{character, Unique indetifier of the respondent.}
#'   \item{trolley_1_resp}{integer, Response for the trolley footbridge pole dilemma. Either 1 for "yes" or 2 for no".}
#'   \item{trolley_1_rate}{integer, Rating of moral acceptability for the trolley footbridge pole dilemma. Using 9 point Likert-type scale, 1 is "Completely unacceptable" while 9 is "Completely acceptable".}
#'   \item{trolley_1_just}{character, Justification for the trolley footbridge pole dilemma.}
#'   \item{trolley_2_resp}{integer, Response for the trolley footbridge switch dilemma. Either 1 for "yes" or 2 for "no".}
#'   \item{trolley_2_rate}{integer, Rating of moral acceptability for the trolley footbridge switch dilemma. Using 9 point Likert-type scale, 1 is "Completely unacceptable" while 9 is "Completely acceptable".}
#'   \item{trolley_2_just}{character, Justification for the trolley footbridge switch dilemma.}
#'   \item{trolley_3_resp}{integer, Response for the trolley standard switch dilemma. Either 1 for "yes" or 2 for "no".}
#'   \item{trolley_3_rate}{integer, Rating of moral acceptability for the trolley standard switch dilemma. Using 9 point Likert-type scale, 1 is "Completely unacceptable" while 9 is "Completely acceptable".}
#'   \item{trolley_3_just}{character, Justification for the trolley standard switch dilemma.}
#'   \item{trolley_4_resp}{integer, Response for the trolley standard footbridge dilemma. Either 1 for "yes" or 2 for "no".}
#'   \item{trolley_4_rate}{integer, Rating of moral acceptability for the trolley standard footbridge dilemma. Using 9 point Likert-type scale, 1 is "Completely unacceptable" while 9 is "Completely acceptable".}
#'   \item{trolley_4_just}{character, Justification for the trolley standard footbridge dilemma.}
#'   \item{trolley_5_resp}{integer, Response for the trolley loop dilemma. Either 1 for "yes" or 2 for "no".}
#'   \item{trolley_5_rate}{integer, Rating of moral acceptability for the trolley loop dilemma. Using 9 point Likert-type scale, 1 is "Completely unacceptable" while 9 is "Completely acceptable".}
#'   \item{trolley_5_just}{character, Justification for the trolley loop dilemma.}
#'   \item{trolley_6_resp}{integer, Response for the trolley obstacle collide dilemma. Either 1 for "yes" or 2 for "no".}
#'   \item{trolley_6_rate}{integer, Rating of moral acceptability for the trolley obstacle collide dilemma. Using 9 point Likert-type scale, 1 is "Completely unacceptable" while 9 is "Completely acceptable".}
#'   \item{trolley_6_just}{character, Justification for the trolley obstacle collide dilemma.}
#'   \item{speedboat_1_resp}{integer, Response for the speedboat footbridge pole dilemma. Either 1 for "yes" or 2 for "no".}
#'   \item{speedboat_1_rate}{integer, Rating of moral acceptability for the speedboat footbridge pole dilemma. Using 9 point Likert-type scale, 1 is "Completely unacceptable" while 9 is "Completely acceptable".}
#'   \item{speedboat_1_just}{character, Justification for the speedboat footbridge pole dilemma.}
#'   \item{speedboat_2_resp}{integer, Response for the speedboat footbridge switch dilemma. Either 1 for "yes" or 2 for "no".}
#'   \item{speedboat_2_rate}{integer, Rating of moral acceptability for the speedboat footbridge switch dilemma. Using 9 point Likert-type scale, 1 is "Completely unacceptable" while 9 is "Completely acceptable".}
#'   \item{speedboat_2_just}{character, Justification for the speedboat footbridge switch dilemma.}
#'   \item{speedboat_3_resp}{integer, Response for the speedboat standard switch dilemma. Either 1 for "yes" or 2 for "no".}
#'   \item{speedboat_3_rate}{integer, Rating of moral acceptability for the speedboatstandard switch dilemma. Using 9 point Likert-type scale, 1 is "Completely unacceptable" while 9 is "Completely acceptable".}
#'   \item{speedboat_3_just}{character, Justification for the speedboat standard switch dilemma.}
#'   \item{speedboat_4_resp}{integer, Response for the speedboat standard footbridge dilemma. Either 1 for "yes" or 2 for "no".}
#'   \item{speedboat_4_rate}{integer, Rating of moral acceptability for the speedboat standard footbridge dilemma. Using 9 point Likert-type scale, 1 is "Completely unacceptable" while 9 is "Completely acceptable".}
#'   \item{speedboat_4_just}{character, Justification for the speedboat standard footbridge dilemma.}
#'   \item{speedboat_5_resp}{integer, Response for the speedboat loop dilemma. Either 1 for "yes" or 2 for "no".}
#'   \item{speedboat_5_rate}{integer, Rating of moral acceptability for the speedboat loop dilemma. Using 9 point Likert-type scale, 1 is "Completely unacceptable" while 9 is "Completely acceptable".}
#'   \item{speedboat_5_just}{character, Justification for the speedboat loop dilemma.}
#'   \item{speedboat_6_resp}{integer, Response for the speedboat obstacle collide dilemma. Either 1 for "yes" or 2 for "no".}
#'   \item{speedboat_6_rate}{integer, Rating of moral acceptability for the speedboat obstacle collide dilemma. Using 9 point Likert-type scale, 1 is "Completely unacceptable" while 9 is "Completely acceptable".}
#'   \item{speedboat_6_just}{character, Justification for the speedboat obstacle collide dilemma.}
#'   \item{trolley_attention}{integer, Response to the trolley attention check question. Respondent had to choose the description of the presented trolley dilemma. Correct responses can be found in the package system data at "R/sysdata.rda" named "correct_answers".}
#'   \item{speedboat_attention}{integer, Response to the speedboat attention check question. Respondent had to choose the description of the presented speedboat dilemma. Correct responses can be found in the package system data at "R/sysdata.rda" named "correct_answers".}
#'   \item{confusion}{integer, Wheter the respondent found the materials confusing. 1 is "I did not find the material on the preceding pages confusing.", 2 is "I found some of the material on the preceding pages confusing at first, but after reading the material carefully and examining the diagram I felt that I understood the material well enough to give reasonable answers to the questions I was asked.", while 3 is "By the time I answered the questions on the preceding page, I was still somewhat confused by the material. I do not think I understood this material well enough to give reasonable answers to the questions I was asked.".}
#'   \item{disbelief}{integer, Whether the respondent found the materials believable. 1 is "I found the description from the preceding pages to be rather realistic.", 2 is "I did not find the description from the preceding pages to be completely realistic, but I “suspended disbelief” and responded under the assumption that it was completely accurate. More specifically, I assumed that Joe’s (or John's) action (or inaction) would affect the situation as the description said it would.", and 3 is "I did not find the description from the preceding pages to be realistic, and my answers reflect my inability to take seriously the description that was given.".}
#'   \item{familiarity}{integer, Familiarity with the task from 1 to 5, where 1 is "absolutely not familiar" and 5 is "absolutely familiar".}
#'   \item{careless_1}{integer, Response to question to the first question to check carelessness. Correct response 2.}
#'   \item{careless_2}{integer, Response to question to the second question to check carelessness. Correct response 2.}
#'   \item{careless_3}{integer, Response to question to the third question to check carelessness. Correct response 1.}
#'   \item{household_income}{numeric, Household income of the respondent in 2019.}
#'   \item{currency_1}{character, Currency of the participants income.}
#'   \item{living_area}{character, Living area of the respondent where 1 is "Rural" and 2 is "Urban".}
#'   \item{oxford_utilitarian_1}{integer, First item of the Oxford utilitarian scale. 1 is "Strongly disagree" and 7 is "Strongly agree".}
#'   \item{individualism_scale_1}{integer, First item of the Individualism scale. 1 is "Never of definitely no" while 9 is "Always or definitely yes".}
#'   \item{religious_rate}{integer, How religious is the respondent from 1 "absolutely not" to 10 "very".}
#'   \item{religion_[1-16]}{integer, Whether the respondent indicated that they are followers of the listed religion. 1 - Agnostic, 2 - Atheist, 3 - Baha'i, 4 - Buddhist, 5 - Chinese Traditional, 6 - Christian, 7 - Christian (specifically Catholic), 8 - Christian (specifically Protestant), 9 - Hindu, 10 - Humanist, 11 - Jewish, 12 - Muslim, 13 - No religion, 14 - Sikh, 15 - Taoist, 16 - Other, please specify}
#'   \item{education_leve}{integer, Highest level of education. 1 is "Below high school", 2 is "High school", 3 is "Bachelor's degree", 4 is "Master's degree", 5 is "Doctoral degree". The labels can differ for certian countries but these are recorded in separate variables.  Levels higher than 2 are considered as higher education.}
#'   \item{education_level_germ}{integer, Highest level of education in the German system. 1 is "Abschluss ohne Abitur/Matura", 2 is "Abitur/Matura", 3 is "Realschulabschluss", 4 is "Hauptschulabschluss", 5 is "Bachelor-Abschluss", 6 is "Master-Abschluss", 7 is "Promotion", 8 is "Habilitation". Levels higher than 4 are considered as higher education.}
#'   \item{age_1}{integer, Age of the respondent, minimum 18. To calculate the actual age of the respondent 17 should be added to the value.}
#'   \item{sex}{integer, Sex of the respondent. 1 - Male; 2 - Female; 3 - Other; 4 - I wish not to answer.}
#'   \item{countr_origin_1}{character, Origin country of the respondent.}
#'   \item{immigrant_back}{integer, Is the respondent from an immigrant background, with 1 "Yes" and 2 "No".}
#'   \item{technical_problems}{integer, Whether the respondent had technical problems during survey completition. 1 is "No" while 2 is "Yes".}
#'   \item{technical_problems_2_TEXT}{character, Description of the technical problem.}
#'   \item{further_comments}{character, Further comments by the respondent.}
#'   \item{native_language}{integer, Was the study presented in the primary language of the respondent with 1 as "Yes" and 2 as "No"}
#'   \item{fluency}{integer, Is the respondent fluent in the language of the survey with 1 as "Yes" and 2 as "No".}
#'   \item{Q_Lang}{character, Language of the survey. Value-key mapping of the variable: English (US) - EN, Argentinian Spanish - ES-ARG, Bahasa Indonesia - INDO, Brazilian Portuguese - PT-BR, Bulgarian - BG, Chilean Spanish - CHI, Chinese (Simplified) - ZH-S, Colombian Spanish - COL-ESP, Czech - CS, Dutch - NL, Ecuadorian Spanish - ES-EC, España - ESP-ESP, French - FR, German - DE, Greek - EL, Hrvatski - CR, Hungarian - HU, Italian - IT, Japanese - JA, Macedonian - MK, Mexican Spanish - ES-MX, Persian - FA, Peruvian Spanish - ES-PE, Polish - PL, Portuguese - PT, Romanian - RO, Russian - RU, Salvador Spanish - ESP-SAL, Slovak - SK, Spanish - ES-ES, Thai - TH, Turkish - TR, Српски - SERB, Tagalog(Philippines - TGL)}
#'   \item{lab}{character, Unique identifier of the lab collecting the given response.}
#'   \item{attention}{, }
#'   \item{scenario1}{character, Which dilemma was shown to the respondent first. Added during data cleaning.}
#'   \item{scenario2}{character, Which dilemma was shown to the respondent second. Added during data cleaning.}
#'   \item{country3}{character, ISO3 country code of the respondents' country.}
#'   \item{Region}{character, Which world region the respondents' country belongs to.}
#'   \item{Age}{integer, Recoded age value during data cleaning.}
#'   \item{Gender}{character, Renamed "sex" variable during data cleaning. Renaming was a mistake, the variable contains the sex and NOT the gender of the respondents.}
#'   \item{Higher education}{character, Recoded values of the "education_leve" variable during data cleaning.}
#'   \item{include_nocareless}{logical, TRUE if respondent passed the carelesness test and FALSE if dont}
#'   \item{include_noconfusion}{logical, TRUE if respondent passed the confusion test and FALSE if dont}
#'   \item{include_nofamiliarity}{logical, TRUE if respondent was not familiar with the task and FALSE if they were}
#'   \item{include_notechproblem}{logical, TRUE if respondent had no technical problem and FALSE if they had}
#'   \item{include_nonativelang}{logical, TRUE if respondent filled out the survey in their native language and FALSE if dont}
#'   \item{include_noproblem}{logical, TRUE if respondent passed all the exclusion criteria (i.e. carelessness, confusion, familiarity, technical probelem, and native language check) and FALSE if dont}
#'   \item{include_withoutfamiliarity}{logical, TRUE if respondent passed all exclusion criteria except familiarity does not matter and FALSE if dont}
#'   \item{include_familiar}{logical, TRUE if respondent passed all exclusion criteria but only including respondents who were familiar with the tasks and FALSE if dont}
#'   \item{include_study1a_attention}{logical, TRUE if respondent passed the attention check for study1a but non of the other attention check applied and FALSE if dont}
#'   \item{include_study1b_attention}{logical, TRUE if respondent passed the attention check for study1b but non of the other attention check applied and FALSE if dont}
#'   \item{include_study1a}{logical, TRUE if respondent passed all of the checks including the attention check for study1a and FALSE if dont}
#'   \item{include_study1b}{logical, TRUE if respondent passed all of the checks including the attention check for study1b and FALSE if dont}
#'   \item{include_study1a_withoutfamiliarity}{logical, TRUE if respondent passed all of the checks including the attention check for study1a but familiarity does not matter and FALSE if dont}
#'   \item{include_study1b_withoutfamiliarity}{logical, TRUE if respondent passed all of the checks including the attention check for study1b but familiarity does not matter and FALSE if dont}
#'   \item{include_study1a_familiar}{logical, TRUE if respondent passed all of the checks including the attention check for study1a but only including respondents familiar with the task and FALSE if dont}
#'   \item{include_study1b_familiar}{logical, TRUE if respondent passed all of the checks including the attention check for study1b but only including respondents familiar with the task and FALSE if dont}
#'   \item{include_study2a_attention}{logical, TRUE if respondent passed the attention check for study2a but non of the other attention check applied  and FALSE if dont}
#'   \item{include_study2b_attention}{logical, TRUE if respondent passed the attention check for study1b but non of the other attention check applied  and FALSE if dont}
#'   \item{include_study2a}{logical, TRUE if respondent passed all of the checks including the attention check for study2a and FALSE if dont}
#'   \item{include_study2b}{logical, TRUE if respondent passed all of the checks including the attention check for study2b and FALSE if dont}
#'   \item{include_study2a_withoutfamiliarity}{logical, TRUE if respondent passed all of the checks including the attention check for study2a but familiarity does not matter and FALSE if dont}
#'   \item{include_study2b_withoutfamiliarity}{logical, TRUE if respondent passed all of the checks including the attention check for study2b but familiarity does not matter and FALSE if dont}
#'   \item{include_study2a_familiar}{logical, TRUE if respondent passed all of the checks including the attention check for study2a but only including respondents familiar with the task and FALSE if dont}
#'   \item{include_study2b_familiar}{logical, TRUE if respondent passed all of the checks including the attention check for study2b but only including respondents familiar with the task and FALSE if dont}
#'}
"trolley"
