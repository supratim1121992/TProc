#' Preprocessing for textual data
#'
#' The function allows users to clean textual data by removing punctuation marks, whitespaces and stop words. A vocabulary and Document Term Matrix are then generated.
#' Digits and other special characters can also be removed and words can be lemmatized to their base forms.
#' Users can also choose to create word clouds for visualization and save the DTM locally.
#' @param text A character vector with the textual data (words/strings).
#' @param rem_num Logical specifing if digits in the text should be removed. FALSE by default.
#' @param rem_spec Logical specifying if special characters (other than the 14 commonly used punctuation marks) such as @, #, & etc should be removed. FALSE by default.
#' @param lemma Logical specifying if the words in the text should be lemmatized to their base forms. FALSE by default.
#' @param lib_stop Libraries to be used for stop word removal.
#' @param add_stop Character vector of user defined stop words. This is optional.
#' @param ngram_min The minimum number of words to be considered for ngrams.
#' @param ngram_max The maximum number of words to be considered for ngrams.
#' @param term_min The minimum number of times a word should be present in the text for it to be recognised as a keyword.
#' @param term_max The maximum number of times a word can be present in the text for it to be recognised as a keyword.
#' @param doc_prop_min Minimum proportion of documents which should contain the term.
#' @param doc_prop_max Maximum proportion of documents which should contain the term.
#' @param doc_cnt_min Minimum number of documents that should contain the term for it ro be considered as a keyword.
#' @param doc_cnt_max Maximum number of documents that can contain the term for it to be considered a keyword.
#' @param voc_cnt_max The maximum number of keywords to be included in the vocabulary. Keywords with higher frequencies are retained.
#' @param write_mat Logical specifying if the Document Term Matrix should be written out as a CSV file. FALSE by default.
#' @param get_wordcloud Logical specifying if a word cloud should be created. FALSE by default.
#' @param wc_term_max The maximum number of terms to be included in the word cloud. Terms with higher frequencies are retained.
#' @param wc_col The color scheme to be used for the word cloud. Color schemes from RColorBrewer are supported.
#' @return A list containing the processed text, the vocabulary (Term Frequency Table) and the DTM.
#' @export

Text_Proc<-function(text,rem_num = F,rem_spec = F,lemma = F,lib_stop = c("snowball","stopwords-iso","smart"),add_stop = NULL,ngram_min = 1,ngram_max = 1,
                    term_min = 1,term_max = Inf,doc_prop_min = 0,doc_prop_max = 1,doc_cnt_min = 1,doc_cnt_max = Inf,voc_cnt_max = Inf,write_mat = F,
                    get_wordcloud = F,wc_term_max = Inf,wc_col = "Dark2"){
  require(data.table)
  require(stringr)
  require(stopwords)
  require(text2vec)
  require(wordcloud)
  require(textstem)

  proc_txt<-text %>% str_to_lower %>%
    str_replace_all(pattern = "\r\n",replacement = " ") %>%
    str_replace_all(pattern = "\n",replacement = " ") %>%
    str_replace_na(replacement = "") %>%
    str_replace_all(pattern = "[\\,\\;\\:\\-\\\"\\(\\[\\.\\?\\)\\!\\]\\/\\']",replacement = " ")

  if(rem_num == T){
    proc_txt<-str_replace_all(string = proc_txt,pattern = "[:digit:]",replacement = " ")
  }

  if(rem_spec == T){
    proc_txt<-str_replace_all(string = proc_txt,pattern = "[^[:alnum:]]",replacement = " ")
  }

  proc_txt<-str_replace_all(string = proc_txt,pattern = "\\s+",replacement = " ") %>%
    str_replace_all(pattern = "^\\s+|\\s+$",replacement = "")

  if(lemma == T){
    proc_txt<-lemmatize_strings(proc_txt)
  }

  stp_word<-character()
  if("snowball" %in% lib_stop){
    stp_word<-c(stp_word,stopwords::stopwords(language = "en",source = "snowball"))
  }

  if("stopwords-iso" %in% lib_stop){
    stp_word<-c(stp_word,stopwords::stopwords(language = "en",source = "stopwords-iso"))
  }

  if("smart" %in% lib_stop){
    stp_word<-c(stp_word,stopwords::stopwords(language = "en",source = "smart"))
  }

  stp_word<-c(stp_word,add_stop)
  stp_word<-unique(stp_word)
  it_tok<-itoken(iterable = proc_txt)
  voc<-create_vocabulary(it = it_tok,ngram = c(ngram_min,ngram_max),stopwords = stp_word) %>%
    prune_vocabulary(term_count_min = term_min,term_count_max = term_max,doc_proportion_min = doc_prop_min,
                     doc_proportion_max = doc_prop_max,doc_count_min = doc_cnt_min,doc_count_max = doc_cnt_max,vocab_term_max = voc_cnt_max)
  vectorizer<-vocab_vectorizer(vocabulary = voc)
  dtm_voc<-create_dtm(it = it_tok,vectorizer = vectorizer)

  res<-vector(mode = "list",length = 3)
  res[[1]]<-proc_txt
  res[[2]]<-as.data.table(voc)
  res[[3]]<-dtm_voc
  names(res)<-c("Cleaned Text","Term Frequency Table","Document Term Matrix")

  if(write_mat == T){
    if(dim(dtm_voc)[1] * dim(dtm_voc)[2] > 10000000){
      cat(paste("\nThe Document Term Matrix is too large and will be written in multiple files in your working directory.",
                getwd(),sep = "\n"))
      ele<-round(x = 10000000/dim(dtm_voc)[2],digits = 0)
      parts<-split(1:dim(dtm_voc)[1],ceiling(seq_along(1:dim(dtm_voc)[1])/ele))
      for(i in 1:length(parts)){
        write.csv(x = as.matrix(res$`Document Term Matrix`[parts[[i]],]),file = paste("dtm_",i,".csv",sep = ""))
        cat("\nPart",i,"saved",sep = " ")
      }
    }
    else {
      write.csv(x = as.matrix(res$`Document Term Matrix`),file = "dtm.csv")
      cat(paste("\nThe Document Term Matrix has been saved in your working directory.",getwd(),sep = "\n"))
    }
  }

  if(get_wordcloud == T){
    fl_nm<-paste(getwd(),"Wordcloud.jpeg",sep = "/")
    jpeg(filename = fl_nm,width = 480,height = 480,quality = 100)
    wcl<-wordcloud(words = voc$term,freq = voc$term_count,max.words = wc_term_max,rot.per = 0.25,
                   colors = brewer.pal(n = 8,name = wc_col),fixed.asp = T)
    dev.off()
    cat(paste("\nThe wordcloud has been saved in your working directory.",getwd(),sep = "\n"))
  }

  return(res)
}
