#Written by Gustavo Quinderé Saraiva

rm(list = ls())
set.seed(123)  # For replicability


cat("The function `iaa()` and its auxiliary functions used to implement this code were adapted from the matchingMarkets library developed by Thilo Klein (https://github.com/thiloklein/matchingMarkets). The following modifications and extensions were made:

1. Compute a lower bound to the proportion of colleges who could end up better off by rendering themselves unacceptable to at least one student. 

2. Introducing the parameter k, which indicates the number of colleges that each student finds acceptable

")



iaa <- function(nStudents=ncol(s.prefs), nColleges=ncol(c.prefs), nSlots=rep(1,nColleges), s.prefs=NULL, c.prefs=NULL, acceptance="immediate", short_match = TRUE,k_bar=nColleges){
  # if(!is.null(seed)){
  #   set.seed(seed)
  # }
  
  #List to store all students who proposed to a each college
  lists_ranking_of_all_students_who_proposed <- vector("list", nColleges)

  ## If 'nColleges' not given, obtain it from nSlots
  if(is.null(nColleges)){
    nColleges <- length(nSlots)
  }
  k_bar=min(nColleges,k_bar)#the maximum number of acceptable choices for students is given by the minimum between the number of schools and the parameter k_bar.
  ## If no prefs given, make them randomly:
  if(is.null(s.prefs)){
    s.prefs <- replicate(n=nStudents,sample(seq(from=1,to=nColleges,by=1)))
  }
  if(is.null(c.prefs)){
    c.prefs <- replicate(n=nColleges,sample(seq(from=1,to=nStudents,by=1)))
  }
  
  ## Consistency checks:
  if( dim(s.prefs)[1] != dim(c.prefs)[2] | dim(s.prefs)[2] != dim(c.prefs)[1] |
      dim(s.prefs)[2] != nStudents | dim(c.prefs)[2] != nColleges |
      dim(c.prefs)[1] != nStudents | dim(s.prefs)[1] != nColleges ){
    stop("'s.prefs' and 'c.prefs' must be of dimensions 'nColleges x nStudents' and 'nStudents x nColleges'!")
  }
  if( length(nSlots) != nColleges | length(nSlots) != dim(c.prefs)[2] ){
    stop("length of 'nSlots' must equal 'nColleges' and the number of columns of 'c.prefs'!")
  }
  
  iter <- 0
  
  s.hist    <- rep(0,length=nStudents)  # number of proposals made
  c.hist    <- lapply(nSlots, function(x) rep(0,length=x))  # current students
  s.singles <- 1:nStudents
  
  s.mat <- matrix(data=1:nStudents,nrow=nStudents,ncol=nColleges,byrow=F)
  
  while(min(s.hist[s.singles]) < k_bar){  # there are as many rounds as maximal preference orders
    # look at market: all unassigned students
    # if history not full (been rejected by all colleges in his prefs)
    # look at unassigned students' history
    # propose to next college on list
    iter         <- iter + 1
    offers       <- NULL
    
    ## Look at unassigned students that have not yet applied to all colleges
    temp.singles <- c(na.omit( s.singles[s.hist[s.singles] < k_bar] ))
    if(length(temp.singles)==0){ # if unassigned students have used up all their offers: stop
      return(finish(s.prefs,c.prefs,iter,c.hist,s.singles,lists_ranking_of_all_students_who_proposed,short_match,nSlots))
    }
    
    ## Add to students' offer history
    for(i in 1:length(temp.singles)){
      s.hist[temp.singles[i]] <- s.hist[temp.singles[i]] + 1  # set history of student i one up.
      if(s.hist[temp.singles[i]] > nColleges){   # Skip student if he has already applied to all colleges
        next()
      }
      offers[i] <- s.prefs[s.hist[temp.singles[i]],temp.singles[i]]  # offer if unassigned i is index of current round college
    }
    
    ##print(paste("Iteration: ",iter))
    approached <- unique(offers)	# index of colleges who received offers
    
    # Dont approach college 0 since it means that the student prefers to stay unmatched
    approached <- approached[!approached == 0]
    approached <- approached[!is.na(approached)]
    
    s.singles  <- sort(s.singles[!s.singles %in% temp.singles])  # reset unassigned students, except for singles who already used up all offers
    
    for(j in approached){
      all_proposers   <- temp.singles[offers==j]
      lists_ranking_of_all_students_who_proposed[[j]] <- c(lists_ranking_of_all_students_who_proposed[[j]], which(c.prefs[,j] %in% all_proposers))#UPDATED
      proposers   <- c.prefs[,j][c.prefs[,j] %in% all_proposers]  # Only keep proposers that are ranked by the approached college
      not_ranked <-  all_proposers[!all_proposers %in% proposers] # Students that are not ranked remain single
      stay.single <- temp.singles[offers==0 | is.na(offers)]	# students who prefer remaining unassigned at current history
      
      for (k in 1:length(proposers)){
        
        # Gale-Shapley:
        if(acceptance == 'deferred'){
          #          if(0 %in% c.hist[[j]] && any(c.prefs[ ,j]==proposers[k])){  # if no history and proposer is on preference list
          if(0 %in% c.hist[[j]] && !is.na(any(c.prefs[ ,j]==proposers[k])) && any(c.prefs[ ,j]==proposers[k])){  # if no history and proposer is on preference list
            
            #c.hist[[j]][c.hist[[j]]==0][1] <- proposers[k]			  # then accept
            c.hist[[j]][match(0, c.hist[[j]])] <- proposers[k]
            
          } else{
            # Compare prosposing student to the students that currently hold an offer
            eval_prop  <- proposer_better(proposer = proposers[k], prefs =  c.prefs, college = j, hist = c.hist)
            
            # If the proposing student is not preferred, reject him
            if(is.na(eval_prop$better) || eval_prop$better == FALSE){
              s.singles <- c(s.singles,proposers[k])	# otherwise k stays unassigned
            } else{ # Otherwise assign him to the seat, that is currently holded by the least preferred student, who becomes unassigned again
              s.singles <- c(s.singles, eval_prop$worst_stud)
              c.hist[[j]][eval_prop$index_worst_stud] <- proposers[k]
              
            }
          }
          # IAA Algorithm:
        } else{
          #         if(0 %in% (c.hist[[j]] & any(c.prefs[ ,j]==proposers[k]))){  # if no history and proposer is on preference list
          if(0 %in% c.hist[[j]] && !is.na(any(c.prefs[ ,j]==proposers[k])) && any(c.prefs[ ,j]==proposers[k])){  # if 0 in history and proposer is on preference list
            
            #c.hist[[j]][c.hist[[j]]==0][1] <- proposers[k]			  # then accept
            c.hist[[j]][match(0, c.hist[[j]])] <- proposers[k]
            
          }else{
            s.singles <- c(s.singles,proposers[k])	# otherwise k stays unassigned
          }
        }
      }
      s.singles <- sort(unique(c(s.singles,stay.single, not_ranked))) #Update singles in every round
    }
    
    if(length(s.singles)==0){	# if no unassigned students left: stop
      #current.match <- sapply(1:nColleges, function(x) s.mat[,x] %in% c.hist[[x]])
      
      return(finish(s.prefs,c.prefs,iter,c.hist,s.singles,lists_ranking_of_all_students_who_proposed,short_match,nSlots))
    }
    current.match <- sapply(1:nColleges, function(x) s.mat[,x] %in% c.hist[[x]])
  }
  return(finish(s.prefs,c.prefs,iter,c.hist,s.singles,lists_ranking_of_all_students_who_proposed,short_match,nSlots))#added as intermediate output lists_ranking_of_all_students_who_proposed
}


# To Sum up and format the output
finish <- function(s.prefs,c.prefs,iter,c.hist,s.singles,lists_ranking_of_all_students_who_proposed,short_match,nSlots){
  nColleges=ncol(c.prefs)
  benefits_from_manipulating_attractiveness=rep(0,nColleges)  
  lists_ranking_of_accepted_students <- vector("list", nColleges)
  if(short_match == FALSE){
    return(list(s.prefs=s.prefs,c.prefs=c.prefs,iterations=iter,matchings=edgefun(x=c.hist),singles=s.singles))
  }
  else {
    # Format matching
    matching <- edgefun(x=c.hist)
    
    free_caps <- lapply(1:ncol(c.prefs), function(col){
      return(nrow(matching[matching$college == col & matching$student == 0,]))
    })
    free_caps <- data.frame(free_caps)
    colnames(free_caps) <- 1:ncol(c.prefs)
    
    matching <- matching[matching$student != 0, ]
    for(j in 1:ncol(c.prefs)){
      if(j %in% matching$college){
        ranking_matched_students=sort(which(c.prefs[,j]%in%matching$student[matching$college==j]))
        if(length(ranking_matched_students)==nSlots[j]){#If a college managed to fill all of its vacancies, let's see if it could have gotten something better:
          ordered_lists_ranking_of_all_students_who_proposed=sort(lists_ranking_of_all_students_who_proposed[[j]])
          if(ordered_lists_ranking_of_all_students_who_proposed[nSlots[j]]<ranking_matched_students[nSlots[j]]){#if the college could have gotten something better by rendering itself unacceptable to some student:
            #print(paste('college',j,'has incentives to reduce its desirability',sep=' '))
            benefits_from_manipulating_attractiveness[j]=1
          }
        }
      }
    }
    
    return(list(s.prefs=s.prefs,c.prefs=c.prefs,iterations=iter,matchings=matching,singles=s.singles, free_cap = free_caps, prop_benefits_misreport=sum(benefits_from_manipulating_attractiveness)/length(benefits_from_manipulating_attractiveness)))#=benefits_from_manipulating_attractiveness
  }
}

## convert match matrix to edgelist
edgefun <- function(x){
  res <- data.frame(college = c(unlist( sapply(1:length(x), function(i){
    rep(i,length(x[[i]]))
  }) )),
  student = unlist(x),
  stringsAsFactors = FALSE)
  #browser()
  res <- with(res, res[order(college, student),])
}

## Compare proposer and current students
proposer_better <- function(proposer, prefs, college, hist){
  rank_proposer <- match(proposer, prefs[, college])
  rank_students <- match(hist[[college]], prefs[, college])
  #index_worst_stud = which.max(rank_students
  #hist[[college]][which.max(rank_students)]
  return(list(better = any(rank_proposer < rank_students), worst_stud = hist[[college]][which.max(rank_students)], index_worst_stud = which.max(rank_students)))
}

#=======USAGE===================
s.prefs <- matrix(c(1,2,3,
                    1,2,3,
                    1,3,2,
                    2,1,3,
                    3,1,2),
                  byrow = FALSE, ncol = 5, nrow = 3)
c.prefs <- matrix(c(1,4,2,3,5,
                    5,2,3,4,1,
                    1,2,3,4,5),
                  byrow = FALSE, ncol = 3, nrow = 5)
nSlots <- c(2,1,1)

## Boston mechanism
 iaa(s.prefs = s.prefs, c.prefs = c.prefs, nSlots = nSlots,k=3)
#===========================================
 

#=================
#====SIMULATIONS==
#================= 
 #the following function generates students' and colleges' preferences
 #theta governs how colleges' utilities are correlated (0 <= theta <= 1)
 generate_random_preferences <- function(phi, m, q,theta) {
   # Arguments:
   # phi: Multiplier for the number of students (total students = phi * m).
   # m: Number of colleges.
   # q: Number of slots per college (fixed for simplicity).
   
   # Total number of students
   n_students <- ceiling(phi * m)
   
   # Number of slots for each college
   n_slots <- rep(q, m)
   
   # Generate random preferences for students over colleges
   # Each student ranks all colleges in random order
   s_prefs <- t(apply(matrix(1:m, nrow = n_students, ncol = m, byrow = TRUE), 1, sample))
   
   # Generate random preferences for colleges over students
   # Each college ranks all students in random order
   
   
   # Generate homogeneous utilities for colleges over students (shared component)
   common_utility_colleges <- matrix(runif(n_students, min = 0, max = 1), nrow = 1, ncol = n_students)
   
   # Generate idiosyncratic utilities for colleges over students (random component)
   idiosyncratic_utility_colleges <- matrix(runif(n_students * m, min = 0, max = 1), nrow = m, ncol = n_students)
   
   # Combine the utilities for colleges over students
   c_utilities <- theta * matrix(rep(common_utility_colleges, m), nrow = m, byrow = TRUE) +
     (1 - theta) * idiosyncratic_utility_colleges
   
   # Convert utilities to ordinal preferences for colleges
   c_prefs <- apply(c_utilities, 1, function(row) order(row, decreasing = TRUE))
   #c_prefs <- t(apply(matrix(1:n_students, nrow = m, ncol = n_students, byrow = TRUE), 1, sample))
   
   # Return results
   return(list(
     s_prefs = s_prefs,
     c_prefs = c_prefs,
     n_slots = n_slots
   ))
 }

 
 sample_size=500
 m=400
 q <- 5      # Each college has 5 slots
 k<-5  #students have at most 5 acceptable choices
 phi_grid=seq(1,10,.1)
 
 theta_grid=seq(0,1,.2)
 
 # Define a color palette for the lines
 colors <- rainbow(length(theta_grid))
 
 #matrix to store final results:
 proportion_misreport=matrix(rep(rep(0,length(phi_grid)),length(theta_grid)),length(theta_grid),length(phi_grid))
 
 #proportion_misreport=rep(0,length(phi_grid))
 for(l in 1:length(theta_grid)){
   theta=theta_grid[l]
 for(i in 1:length(phi_grid)){
   print(paste('iteration: ',i,sel=''))
   phi=phi_grid[i]
   for(s in 1:sample_size){
     random_prefs <- generate_random_preferences(phi, m, q,theta)
     #cat("Student Preferences:\n")
     s.prefs= t(random_prefs$s_prefs)
     
     #cat("\nCollege Preferences:\n")
     c.prefs=(random_prefs$c_prefs) 
     
     #cat("\nSlots per College:\n")
     nSlots=random_prefs$n_slots
     
     proportion_misreport_i=iaa(s.prefs = s.prefs, c.prefs = c.prefs, nSlots = nSlots,k=k)$prop_benefits_misreport
     proportion_misreport[l,i]=proportion_misreport[l,i]+proportion_misreport_i
   }
   proportion_misreport[l,i]=proportion_misreport[l,i]/sample_size#to get the average
 }
   if(l==1){
   pdf('incentives_manipulate_Boston.pdf',width=7,height=5)
   plot(phi_grid,proportion_misreport[l,],type='l',xlab=expression(phi),ylab='Prob. college has incentives to manipulate',cex.lab=1.3,lwd=2,cex.axis=1.2,ylim=c(0,1),col = colors[l])
   # Add a blue dashed horizontal line at y = 1.0
   abline(h = 1.0, col = "blue", lty = 2)
   }else{
     lines(phi_grid,proportion_misreport[l,],type='l',cex.lab=1.3,lwd=2,cex.axis=1.2,col = colors[l])
     # Add a blue dashed horizontal line at y = 1.0
   }
 }
 # Add legend after plotting all lines
 legend_labels <- lapply(theta_grid, function(theta) bquote(theta == .(theta)))
 legend("bottomright", legend = legend_labels, col = colors, lwd = 2, cex = 1)
 #legend("bottomright", legend = paste0(expression(theta), theta_grid), col = colors, lwd = 2, cex = 1)
   dev.off()


#Save data in csv in case you want to adjust the layout of the plot later:
write.csv(proportion_misreport, file='proportion_misreport.csv',row.names=theta_grid)   