nparcompGrouping <- function(nparcomp.res, pval=0.05, capital_letter = FALSE){

  if(!is.list(nparcomp.res)) return(message("nparcomp.res is wrong"))
  if(is.null(nparcomp.res$Data.Info)) return(message("nparcomp.res is not include input data"))
  if(is.null(nparcomp.res$Analysis)) return(message("nparcomp.res is not include paired-wised analysis"))
  if(pval < min(nparcomp.res$Analysis$p.Value)) return(message("There are no significnat in nparcomp.res"))
  if(length(pval) != 1) return(message("pval must be a number"))
  if(!is.numeric(pval)) return(message("pval must be a number"))
  if(pval >= 1 | pval <= 0) return(message("pval must be 0 to 1.\n 0.05 is recommended"))
  if(!is.logical(capital_letter)) return(message("capital_letter must be TRUE or FALSE"))

  nparcomp.res_input <- nparcomp.res$input$data
  nparcomp.res_group <- nparcomp.res$Analysis
  nparcomp.res_sig <- subset(nparcomp.res , nparcomp.res $p.Value <pval)

  nparcomp.res_group <- subset(nparcomp.res_group, p.Value > pval)

  colnames(nparcomp.res_input) <- c("variable", "value")

  data_mean <- nparcomp.res_input %>%
    group_by(variable) %>%
    summarise(Mean = mean(value))

  data_mean <- data_mean[order(data_mean$Mean, decreasing = T),]

  nparcom_sample <- unique(data_mean$variable)

  for( i in 1 : length(nparcom_sample)){
    if(i == 1){
      group <- data.frame(str_detect(nparcomp.res_group$Comparison, nparcom_sample[i]))
    }else{
      group <- cbind(i=group, str_detect(nparcomp.res_group$Comparison, nparcom_sample[i]))
    }
  }

  colnames(group) <- nparcom_sample
  row.names(group) <- nparcomp.res_group$Comparison

  grouping <- list()
  for( i in 1:length(nparcom_sample)){
    A <- row.names(group)[group[,nparcom_sample[i]]]
    A <- unlist(str_split(A, " , "))
    A <- gsub(" ", "",A)
    A <- gsub("^p", "", A)
    A <- gsub("[(]", "", A)
    A <- gsub("[)]", "", A)
    A <- unique(A)
    grouping[[i]]  <- A[order(A)]
  }

  grouping

  for(i in length(grouping):1){
    if(is_empty(grouping[[i]])){
      grouping[[i]] <- NULL
    }
  }


  comp_overlap <- function(x, y){
    sum(x %in% intersect(x, y)) == length(x) &
      sum(y %in% intersect(x, y)) == length(y)
  }

  group_mat <- c()
  for(i in 1:length(grouping)){
    for(j in 1:length(grouping)){
      group_mat <- c(group_mat, comp_overlap(x=grouping[[i]],
                                             y=grouping[[j]])
      )
    }
  }

  group_mat <- as.data.frame(matrix(group_mat, nrow=length(grouping)))
  group_mat <- group_mat[, duplicated(group_mat)]
  overlap_group <- as.integer(gsub("V", "", colnames(group_mat)))

  for(i in 1:length(overlap_group)){
    grouping[overlap_group[i]-i+1]<-NULL
  }

  names(grouping) <- LETTERS[1:length(grouping)]

  names(nparcom_sample) <- nparcom_sample

  nparcom_group <- nparcom_sample

  for(i in 1:length(nparcom_group )){
    nparcom_group [names(nparcom_group )[i]] <- ""
    for(j in 1:length(grouping)){
      if(names(nparcom_group )[i] %in% grouping[[j]]){
        nparcom_group [i] <- paste0(nparcom_group [i], names(grouping[j]))
      }
    }
  }

  nparcom_group

  while("" %in%  nparcom_group){
    not_group <- which(nparcom_group == "")[1]
    front_group <- str_split(nparcom_group[1:not_group-1], "")
    front_group <- unlist(front_group)
    front_group_max <- max(which(LETTERS%in%front_group))
    nparcom_group[not_group] <- LETTERS[front_group_max+1]

    if(not_group+1 <= length(nparcom_group)){
      for(i in length(LETTERS):2){
        nparcom_group[(not_group+1):length(nparcom_group)] <- gsub(LETTERS[i], LETTERS[i+1], nparcom_group[(not_group+1):length(nparcom_group)] )
      }
    }
  }


  data2 <- data.frame(nparcom_group )
  data2$sample <- rownames(data2)

  data_max <- nparcomp.res_input %>%
    group_by(variable) %>%
    summarise(Mean = mean(value), Max=max(value), Min=min(value), Std.Dev = sd(value))

  data_max <- data_max[order(data_max$Mean, decreasing = T),]

  results <- cbind(data2, data_max[,-1])

  if(capital_letter == F){
    results$nparcom_group <- tolower(results$nparcom_group)
  }
  print(results[,-2])
}
