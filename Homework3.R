###
### Title: ICPSR Homework 3 -- Data Science and Text Analysis
### Author: Alex Tran (Quang Anh Ngo Tran)
### Date: August 3rd, 2023
###

## Setting working directory
setwd("C:/Users/linht/Downloads/Data Science and Text Analysis/Project")

##### 1. Loading in the data #####

#Loading in packages
library(phrasemachine)
library(quanteda)
library(dplyr)
library(stringr)
library(udpipe) 
library(flextable)
library(NLP)
library(ggplot2)
library(readtext)
library(stopwords)
library(quanteda.textplots)
library(preText)
library(stm)
library(readr)
library(readtext)
library(quanteda)
library(text2vec)
library(conText)
library(ggplot2)
library(ggrepel)

#Loading in the data

data_dir <- "C:/Users/linht/Downloads/Data Science and Text Analysis/Project/"

cohasample <- readtext(paste0(data_dir, "text/*"), # Import the raw txt files, drawing document metadata from the txt file names
                       docvarsfrom = "filenames", 
                       dvsep="_", 
                       docvarnames = c("Genre", "Year", "ID"))

#Making a corpus out of the texts
cohasample <- corpus(cohasample, text_field = "text")
docvars(cohasample)
cohasample$Year <- as.numeric(cohasample$Year)

#Getting some summary statistics
summary(cohasample) 

##### 2. Using preText to make informed decisions about preprocessing #####
## I don't recommend running this part on your computer - it takes about 20 minutes.

# generate factorial preprocessing specifications
preprocessed_documents <- factorial_preprocessing(
  cohasample,
  use_ngrams = FALSE, 
  infrequent_term_threshold = 0.05, #this is somewhat random, may increase or decrease
  verbose = FALSE) #letting user know the progress of preprocessing

# generate pretext scores
preText_results <- preText(
  preprocessed_documents,
  dataset_name = "COHA_sample_preprocessed",
  distance_method = "cosine", # can also use Euclidean distance method.
  num_comparisons = 50, # standard for publication standards.
  verbose = FALSE)


# create a preText plot ranking specifications
preText_score_plot(preText_results)

# see which features "matter"
regression_coefficient_plot(preText_results,
                            remove_intercept = TRUE)

## Based on these results, it seems that removing stopwords and punctuations seem to significantly change the corpus. However, since punctuations do not contribute to our understanding of gender stereotypes, I have decided to remove them. As for stopwords, I decided to retain "he" and "she" and eliminate other stopwords since they do not contribute to our understanding of gender stereotypes either.

##### 3. Tokenization #####

# Tokenize 
my_corp_tokens <- tokens(tolower(cohasample), 
                         what = "word",
                         remove_punct = TRUE, 
                         remove_symbols = TRUE,
                         remove_numbers = TRUE, 
                         remove_url = TRUE, 
                         remove_separators = TRUE, 
                         include_docvars = TRUE)

dfm_default <- dfm(my_corp_tokens)

#adding some meaningless words into my list of stopwords to remove
custom_stopwords <- c(stopwords(), "s", "p", "n't", "--", "d", "z", "ll", "m", "ve", "nbsp") custom_stopwords <- custom_stopwords[! custom_stopwords %in% c("he", "she")]

# Remove custom stopwords
dfm_nostopwords  <- dfm_remove(dfm_default,
                               custom_stopwords)

# Stem words
dfm_cleaned <- dfm_wordstem(dfm_nostopwords)

# Remove very infrequent and very frequent terms in the unsupervised world
doc_term_matrix <- dfm_trim(dfm_cleaned,
                            min_termfreq = 10,
                            max_docfreq = 1000)

##### 4. Structural Topic Modeling #####

## For the purpose of this assignment, I will only be examining documents from 1950-2009.

dfm_short <- dfm_subset(doc_term_matrix, Year %in% 1950:2009) # subset for the years I want

# --- convert our quanteda DFM to STM's format
cohashort <- quanteda::convert(dfm_short, to = "stm") # "out" is simply the way STM refers to its core collection of data
coha_docs <- cohashort$documents # pull out the indexed terms that appear in each document
coha_vocab <- cohashort$vocab # pull out unique terms
coha_meta <- cohashort$meta # pull out metadata (i.e. docvars, i.e. covariates)

# --- searchK function for a data-driven approach to select the number of topics
search_k_results <- searchK(documents = cohashort$documents,  # our documents
                            vocab = cohashort$vocab, # our unique terms
                            K = c(2:5), # the range of K's we'd like to consider
                            prevalence =~ Genre + s(Year),  
                            data = coha_meta,
                            seed = 123)

# plot the results
plot(search_k_results)
# held-out likelihood: higher = better - should be 3 topics or more
# residuals: lower = better - should be 3 topics or more
# coherence: higher = better; measure of coherence of words in topic - 2-3 topics is best
# lower bound is akin to model's internal measure of fit; it's the approx of lower bound on marginal likelihood 

##### 4.1. 3 topic STM #####

# --- fit a structural topic model
stm_fit <- stm(documents = cohashort$documents, # documents
               vocab = cohashort$vocab,  # terms
               K = 3, # number of topics
               prevalence =~ Genre + s(Year), # a regression equation that models genre and year covariates
               data = coha_meta, 
               init.type = "Spectral", # could also choose "LDA" here, but Spectral is faster
               seed = 123) # set your seed for replicability, results can change by seed (similar to bootstrapping)

summary(stm_fit)

# --- assess topic quality

# in short, we like topics that maximize exclusivity (i.e. words are sorted into well-defined
# groups, the higher the better) and semantic coherence (i.e. topics are coherent; top words in topic tend to co-appear in a given doc, the higher the better)
topicQuality(model=stm_fit, documents=coha_docs)

# --- plot topic proportions
plot.STM(stm_fit,
         type="summary",
         labeltype = "frex",
         n = 3)

# --- correlation between topics
# topicCorr estimates the correlations between topics, which is handy for understand how topics relate/co-appear in a doc
topic_correlations <- topicCorr(stm_fit)
topic_correlations$cor

# --- estimate the regression output for our fitted STM
coha_effects <- estimateEffect(1:3 ~ Genre + s(Year), # our regression specification
                                stm_fit, # our fitted STM
                                meta = coha_meta) # our metadata saved above when we converted from quanteda

summary(coha_effects)

# --- plot topics by covariate

plot.estimateEffect(coha_effects, 
                    covariate = "Genre",
                    model = stm_fit, 
                    method = "difference",
                    topics = c(1:3),
                    cov.value1 = "mag", 
                    cov.value2 = "news",
                    xlab = "Estimated Difference in Topic Proportion", 
                    xlim = c(-.8, .3))


plot.estimateEffect(coha_effects,
                    covariate = "Year",
                    method = "continuous",
                    topics = 1:3)

##### 5. Word Embeddings #####

#Reloading the texts
cohasample <- readtext(paste0(data_dir, "text/*"), # Import the raw txt files, drawing document metadata from the txt file names
                       docvarsfrom = "filenames", 
                       dvsep="_", 
                       docvarnames = c("Genre", "Year", "ID"))

coha_docs <- cohasample$text # dump ALL of the raw texts into an object

# --- Fit GloVe Model --- #

#------------------ Without resampling ------------------------------------

tok <- function(x) {word_tokenizer(x)} # create word tokenizer function # %>% lapply( function(x) SnowballC::wordStem(x, language="en"))

coha_sample <- coha_docs[sample(1:length(coha_docs), length(coha_docs), replace = T)] # resample *with replacement* from the underlying corpus to create new corpora, each of which contains the same number of docs as the original

coha_tokens <- coha_sample  %>% tolower %>% tok   # word tokenization and conversion to lowercase

it <- itoken(coha_tokens, progressbar = F) # create vocabulary of simple unigrams

coha_vocab <- create_vocabulary(it) %>% prune_vocabulary(term_count_min = 5L)

vectorizer <- vocab_vectorizer(coha_vocab) # create vocabulary vectorizer function

tcm <- create_tcm(it, vectorizer, skip_grams_window = 5L) # create term co-occurrence matrix

# --- Hyperparameters:

RcppParallel::setThreadOptions(1)
set.seed(42)

glove <- GlobalVectors$new(rank = 300,
                           x_max = 15) #maximum number of occurrences

wv_main <- glove$fit_transform(tcm, n_iter = 50, #50 or 100
                               convergence_tol = 0.001, 
                               n_threads = 1, 
                               learning_rate = 0.15) # provides "main" vectors
## The output of loss seem to decrease much slower starting at the 40th iterations. Thus, during the resampling section, I will use 50 iterations.

wv_context <- glove$components # provides "context" vectors

word_vectors <- wv_main + t(wv_context) # original GloVe paper says to average or sum main and context vectors

# Make a men vs women dimensions 
  men_women_dim <- colMeans(rbind(word_vectors["man",],
                                  word_vectors["men",],
                                  word_vectors["he",],
                                  word_vectors["male",],
                                  word_vectors["boy",])) - 
    colMeans(rbind(word_vectors["woman",],
                   word_vectors["women",],
                   word_vectors["she",],
                   word_vectors["female",],
                   word_vectors["girl",]))
  
  # --- "project" onto this dimension using cosine similarity
  results_df <- data.frame(masculine = lsa::cosine(word_vectors["masculine",],
                                              men_women_dim),
                           feminine = lsa::cosine(word_vectors["feminine",],
                                             men_women_dim),
                           competitive = lsa::cosine(word_vectors["competitive",],
                                                men_women_dim),
                           aggressive = lsa::cosine(word_vectors["aggressive",],
                                              men_women_dim),
                           dominant = lsa::cosine(word_vectors["dominant",],
                                              men_women_dim),
                           muscular = lsa::cosine(word_vectors["muscular",],
                                                       men_women_dim),
                           rugged = lsa::cosine(word_vectors["rugged",],
                                              men_women_dim),
                           affectionate = lsa::cosine(word_vectors["affectionate",],
                                                           men_women_dim),
                           gentle = lsa::cosine(word_vectors["gentle",],
                                              men_women_dim),
                           warm = lsa::cosine(word_vectors["warm",],
                                              men_women_dim),
                           expressive = lsa::cosine(word_vectors["expressive",],
                                                 men_women_dim),
                           creative = lsa::cosine(word_vectors["creative",],
                                                 men_women_dim))
  
  library(tidyr)
  df_long <- pivot_longer(results_df, cols = c(masculine, feminine, competitive, aggressive, dominant, muscular, rugged, affectionate, gentle, warm, expressive, creative), names_to = "Category", values_to = "Value")
  

# plot the mean cosine est with nonparametric CI
ggplot(df_long, aes(x = Value, y =  Category)) +
  geom_point(size = 5) +
  geom_vline(xintercept = 0) +
  theme_bw() +
  labs(x = "Women-Men Dimension", y = NULL) +
  theme(axis.title.x = element_text(size = 16, margin = margin(t=12)),
        axis.text.x = element_text(size = 14),
        axis.text.y = element_text(size = 14))


#---------------------------------I resample the underlying docs and fit 40 models which will allow me to construct nonparametric 95% confidence intervals----------------------------

for(k in 1:40){
  
  tok <- function(x) {word_tokenizer(x)} # create word tokenizer function # %>% lapply( function(x) SnowballC::wordStem(x, language="en"))
  
  coha_sample <- coha_docs[sample(1:length(coha_docs), length(coha_docs), replace = T)] # resample *with replacement* from the underlying corpus to create new corpora, each of which contains the same number of docs as the original
  
  coha_tokens <- coha_sample  %>% tolower %>% tok   # word tokenization and conversion to lowercase
  
  it <- itoken(coha_tokens, progressbar = F) # create vocabulary of simple unigrams
  
  coha_vocab <- create_vocabulary(it) %>% prune_vocabulary(term_count_min = 5L)
  
  vectorizer <- vocab_vectorizer(coha_vocab) # create vocabulary vectorizer function
  
  tcm <- create_tcm(it, vectorizer, skip_grams_window = 5L) # create term co-occurrence matrix
  
  # --- Hyperparameters:
  
  RcppParallel::setThreadOptions(1)
  set.seed(42)
  
  glove <- GlobalVectors$new(rank = 300,
                             x_max = 15) #maximum number of occurrences
  
  wv_main <- glove$fit_transform(tcm, n_iter = 50, #50 or 100
                                 convergence_tol = 0.001, 
                                 n_threads = 1, 
                                 learning_rate = 0.15) # provides "main" vectors
  
  wv_context <- glove$components # provides "context" vectors
  
  word_vectors <- wv_main + t(wv_context) # original GloVe paper says to average or sum main and context vectors
  
  saveRDS(word_vectors,
          file = paste("Resamples/", "resample", k, ".rds", sep = ""))
  
}

# ----- intrinsic model evaluation ----

find_nns(target_embedding = word_vectors["bad",],
            pre_trained = word_vectors, N = 6,
            candidates = NULL, norm = "l2", stem = FALSE)

word_vectors<-readRDS("Resamples/resample1.rds")

# --- calculate cosine similarities and CIs
# next I calculate the cosine sims seperately for each resample and use those similarities to calculate nonparametric confidence intervals, as in kozlowski et al (2019) and rathbun and pomeroy (2022)

file_direct <- "Resamples/"
files_list <- list.files("Resamples/")

# Make a men vs women dimensions 
results_df <- data.frame()
for(i in 1:length(files_list)){ #there are 20 resamples here. Load in 1 by 1
  
  word_vectors_i <- readRDS(paste(file_direct,files_list[i], sep = ""))
  
  # --- create a dimension
  men_women_dim <- colMeans(rbind(word_vectors_i["man",],
                                  word_vectors_i["men",],
                                  word_vectors_i["he",],
                                  word_vectors_i["male",],
                                  word_vectors_i["boy",])) - 
                   colMeans(rbind(word_vectors_i["woman",],
                                  word_vectors_i["women",],
                                  word_vectors_i["she",],
                                  word_vectors_i["female",],
                                  word_vectors_i["girl",]))
  
  # --- "project" onto this dimension using cosine similarity
  results_df <- rbind(results_df,
                      data.frame(masc = lsa::cosine(word_vectors_i["masculine",],
                                                       men_women_dim),
                                 creative = lsa::cosine(word_vectors_i["creative",],
                                                    men_women_dim),
                                 affect = lsa::cosine(word_vectors_i["affectionate",],
                                                        men_women_dim),
                                 aggressive = lsa::cosine(word_vectors_i["aggressive",],
                                                                 men_women_dim),
                                 competitive = lsa::cosine(word_vectors_i["competitive",],
                                                          men_women_dim),
                                 resample_i = i))
}
  
# store the key information for plotting below in a dataframe
plot_df <- data.frame(ci_low = sort(results_df$creative)[2],
                      ci_high = sort(results_df$creative)[19],
                      cos_mean = mean(results_df$creative),
                      term = "creative")

# plot the mean cosine est with nonparametric CI
ggplot(plot_df, aes(x = cos_mean, y = term)) +
  geom_linerange(aes(xmin = ci_low, xmax = ci_high),  size = 1.8, color = "gray40") +
  geom_point(size = 5) +
  geom_vline(xintercept = 0) +
  theme_bw() +
  labs(x = "Women-Men Dimension", y = NULL) +
  coord_cartesian(xlim = c(-.2,.2)) + 
  theme(axis.title.x = element_text(size = 16, margin = margin(t=12)),
        axis.text.x = element_text(size = 14),
        axis.text.y = element_text(size = 14))

##### Final words: Thank you for an amazing class. I hope we get to keep in touch. I appreciate all the things I have learned with you in class and the papers you shared. Best of luck and may our paths cross once again. If you ever want to communicate with me, feel free to send me an email at quatran@iu.edu ####