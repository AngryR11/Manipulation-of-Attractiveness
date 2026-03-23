#Written by Gustavo Quinderé Saraiva

rm(list = ls())
set.seed(123)  # For replicability

library(igraph)


cat("The function `iaa()` and its auxiliary functions used to implement this code were adapted from the matchingMarkets library developed by Thilo Klein (https://github.com/thiloklein/matchingMarkets). The following modifications and extensions were made:

1. Compute a lower bound to the proportion of colleges who could end up better off by rendering themselves unacceptable to at least one student. 

2. Introducing the parameter k, which indicates the number of colleges that each student finds acceptable

")


#Boston, DA and AR mechanism
iaa <- function(nStudents=ncol(s.prefs), nColleges=ncol(c.prefs), nSlots=rep(1,nColleges), s.prefs=NULL, c.prefs=NULL, c.prefs_legal=c.prefs,acceptance="immediate", short_match = TRUE,k_bar=nColleges){

  
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
      return(finish(s.prefs,c.prefs,c.prefs_legal,iter,c.hist,s.singles,lists_ranking_of_all_students_who_proposed,short_match,nSlots))
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
      proposers  <- c.prefs_legal[,j][c.prefs_legal[,j] %in% all_proposers]  # Only keep proposers that are ranked by the approached college
      not_ranked <-  all_proposers[!all_proposers %in% proposers] # Students that are not ranked remain single
      stay.single<- temp.singles[offers==0 | is.na(offers)]	# students who prefer remaining unassigned at current history
      
      for (k in 1:length(proposers)){
        
        # Gale-Shapley:
        if(acceptance == 'deferred'){
          #          if(0 %in% c.hist[[j]] && any(c.prefs[ ,j]==proposers[k])){  # if no history and proposer is on preference list
          if(0 %in% c.hist[[j]] && !is.na(any(c.prefs_legal[ ,j]==proposers[k])) && any(c.prefs_legal[ ,j]==proposers[k])){  # if no history and proposer is on preference list
            
            #c.hist[[j]][c.hist[[j]]==0][1] <- proposers[k]			  # then accept
            c.hist[[j]][match(0, c.hist[[j]])] <- proposers[k]
            
          } else{
            # Compare prosposing student to the students that currently hold an offer
            eval_prop  <- proposer_better(proposer = proposers[k], prefs =  c.prefs_legal, college = j, hist = c.hist)
            
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
          if(0 %in% c.hist[[j]] && !is.na(any(c.prefs_legal[ ,j]==proposers[k])) && any(c.prefs_legal[ ,j]==proposers[k])){  # if 0 in history and proposer is on preference list
            
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
      
      return(finish(s.prefs,c.prefs,c.prefs_legal,iter,c.hist,s.singles,lists_ranking_of_all_students_who_proposed,short_match,nSlots))
    }
    current.match <- sapply(1:nColleges, function(x) s.mat[,x] %in% c.hist[[x]])
  }
  return(finish(s.prefs,c.prefs,c.prefs_legal,iter,c.hist,s.singles,lists_ranking_of_all_students_who_proposed,short_match,nSlots))#added as intermediate output lists_ranking_of_all_students_who_proposed
}




# To Sum up and format the output
finish <- function(s.prefs,c.prefs,c.prefs_legal=c.prefs,iter,c.hist,s.singles,lists_ranking_of_all_students_who_proposed,short_match,nSlots){
  nColleges=ncol(c.prefs)
  benefits_from_manipulating_attractiveness=rep(0,nColleges)  
  lists_ranking_of_accepted_students <- vector("list", nColleges)
  if(short_match == FALSE){
    return(list(s.prefs=s.prefs,c.prefs=c.prefs,c.prefs_legal=c.prefs_legal,iterations=iter,matchings=edgefun(x=c.hist),singles=s.singles))
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
          if(ordered_lists_ranking_of_all_students_who_proposed[nSlots[j]]<ranking_matched_students[nSlots[j]]){#if the college could have gotten something better by rendering itself unacceptable to student j:
            #print(paste('college',j,'has incentives to reduce its desirability',sep=' '))
            benefits_from_manipulating_attractiveness[j]=1
          }
        }
      }
    }
    
    return(list(s.prefs=s.prefs,c.prefs=c.prefs,c.prefs_legal=c.prefs_legal,iterations=iter,matchings=matching,singles=s.singles, free_cap = free_caps, prop_benefits_misreport=sum(benefits_from_manipulating_attractiveness)/length(benefits_from_manipulating_attractiveness)))#=benefits_from_manipulating_attractiveness
  }
}


#=============================================
#The TTC mechanism must be treated separately:
#=============================================
find_cycles<-function(pointers){
  # Convert pointers to a data frame of edges
  edges <- data.frame(from = names(pointers), to = pointers)
  
  # Create igraph object
  g <- graph_from_data_frame(edges, directed = TRUE)
  
  
  # Detect strongly connected components (SCCs)
  scc <- clusters(g, mode = "strong")
  
  
  
  # Extract cycles with detailed pointer information
  cycles <- lapply(which(scc$csize > 1), function(c) {
    # Get nodes in the cycle
    cycle_nodes <- which(scc$membership == c)
    
    # Create a subgraph for the cycle
    subgraph <- induced_subgraph(g, vids = cycle_nodes)
    
    # Extract edges in the cycle
    cycle_edges <- as_data_frame(subgraph, what = "edges")
    
    # Ensure numeric output
    cycle_edges[] <- lapply(cycle_edges, as.numeric)
    
    return(cycle_edges)
  })
  
  # Name the cycles for clarity
  names(cycles) <- paste("Cycle", seq_along(cycles))
  
  
  
  return(cycles)
}

top_trading_cycles <- function(s.prefs, c.prefs, c.real_preferences,nColleges=ncol(c.prefs),nSlots, k_bar=nColleges) {
  
  #List to store all students who proposed to a each college:
  lists_ranking_of_all_students_who_proposed <- vector("list", nColleges)
  
  #Input validation
  student_vector=c()#to store students
  student_match=c()#to store the match from each student
  
  nStudents <- ncol(s.prefs)
  nColleges <- ncol(c.prefs)
  
  k_bar=min(nColleges,k_bar)
  
  if (length(nSlots) != nColleges) {
    stop("Length of nSlots must equal the number of columns in c.prefs.")
  }
  
  #This ensures that colleges and students share different indexes (useful to detect cycles):
  s.prefs <- s.prefs + nStudents
  college_grid<-(nStudents+1):(nColleges+ nStudents)
  
  
  #Only look at each students' top k_bar choices (the number of acceptable choices from each student):
  s.prefs=s.prefs[1:k_bar,]
  
  #Initialize unmatched students and open vacancies
  unmatched_students <- 1:nStudents
  college_vacancies <- nSlots#we will iterate this vector
  matching <- matrix(0, nrow = nStudents, ncol = nColleges)  # Final matching matrix
  
  iteration=0
  
  while (length(unmatched_students) > 0 & sum(college_vacancies)>0) {
    #print(paste('iteration: ', iteration,sep=''))
    # Step 1: Each student points to their most preferred college
    student_pointers <- sapply(unmatched_students, function(student) {
      # Filter acceptable colleges for the student
      acceptable_colleges <- intersect(s.prefs[,student], college_grid[which( college_vacancies > 0)])
      if (length(acceptable_colleges) == 0){
        # Student prefers to stay unmatched
        return(student)  # Point to themselves
      } else {
        # Return the most preferred acceptable college
        return(s.prefs[which.min(!(s.prefs[, student] %in% (acceptable_colleges))),student])#s.prefs[(acceptable_colleges-nStudents), student][1])
      }
    })
    
    # Step 2: Each college points to their most preferred student
    college_pointers <- sapply(college_grid, function(college) {
      if (college_vacancies[(college-nStudents)] == 0) {
        return(college)  # No open slots, college points at himself
      }
      # Get unmatched students who consider A college acceptable (not necessarily college c)
      
      students_considering_college <- unmatched_students#[student_pointers == college]
      if (length(students_considering_college) == 0 ) {
        # College points to itself if no students point to it
        return(college)
      } else {
        # Return the most preferred student
        return(c.prefs[which.min(!c.prefs[,(college-nStudents)]%in% students_considering_college),(college-nStudents)])
      }
    })
    
    # Combine all pointers into a directed graph
    pointers <- c(student_pointers, college_pointers)
    names(pointers) <- c(unmatched_students, college_grid)
    
    for(c in student_pointers){
      students_pointing_college_c=unmatched_students[student_pointers==c]
      ranking_students_pointing_college_c=sort(which(c.prefs[,(c-nStudents)]%in%students_pointing_college_c))#UPDATED
      lists_ranking_of_all_students_who_proposed[[(c-nStudents)]]=unique(c(lists_ranking_of_all_students_who_proposed[[(c-nStudents)]],ranking_students_pointing_college_c))#UPDATED
    }
    
    
    cycles_round_i=find_cycles(pointers)
    
    # Merge all cycles into a single data frame
    cycles_round_i <- do.call(rbind, cycles_round_i)
    
    
    
    students_in_cycle=cycles_round_i$from[cycles_round_i$from<=nStudents]
    match_from_each_student_in_cycle=cycles_round_i$to[cycles_round_i$from<=nStudents]
    
    #record the matches from that round
    student_vector=c(student_vector,students_in_cycle)#to store students
    student_match=c(student_match,match_from_each_student_in_cycle)#to store the match from each student
    
    
    
    #update the set of unmatched students:
    unmatched_students=setdiff(unmatched_students,students_in_cycle)
    #update the number of vacancies from each matched college:
    matched_colleges_index=match_from_each_student_in_cycle-nStudents
    college_vacancies[matched_colleges_index]=college_vacancies[matched_colleges_index]-1
    
    
    #Remove students who have exhausted all of their acceptable choices:
    student_has_exhausted_all_acceptable_options<-function(student) {
      # Filter acceptable colleges for the student
      acceptable_colleges <- intersect(s.prefs[,student], college_grid[which( college_vacancies > 0)])
      if (length(acceptable_colleges) == 0){
        # Student prefers to stay unmatched
        return(FALSE)  # Point to themselves
      } else {
        # Return the most preferred acceptable college
        return(TRUE)
      }
    }
    
    if(length(unmatched_students)>0){
      students_who_exhausted_options<-sapply(unmatched_students,student_has_exhausted_all_acceptable_options)
      unmatched_students<-unmatched_students[students_who_exhausted_options]
    }
    
    
    iteration=iteration+1
  }
  
  #putting accepted students in order (not necessary)
  lists_ranking_of_all_students_who_proposed=lapply(lists_ranking_of_all_students_who_proposed,sort)
  
  
  #getting the list of students matched with a college:
  
  final_match_function=cbind(student_vector,student_match)
  
  final_output=finish_TTC(s.prefs,c.prefs,c.real_preferences,college_vacancies,final_match_function,lists_ranking_of_all_students_who_proposed,nSlots)
  
  return(final_output)
}

finish_TTC <- function(s.prefs,c.prefs,c.real_preferences,college_vacancies,final_match_function,lists_ranking_of_all_students_who_proposed,nSlots){
  nColleges=ncol(c.prefs)
  nStudents=ncol(s.prefs)
  benefits_from_manipulating_attractiveness=rep(0,nColleges)  
  lists_ranking_of_accepted_students <- vector("list", nColleges)
  
  matching<-data.frame(final_match_function)
  
  
  colleges=1:ncol(c.prefs)+nStudents
  
  matching <- matching[matching$student_vector!= 0, ]
  
  for(j in colleges){
    if(j %in% matching$student_match){
      ranking_matched_students=sort(which(c.real_preferences[,(j-nStudents)]%in%matching$student_vector[matching$student_match==j]))
      if(college_vacancies[(j-nStudents)]==0){#If a college managed to fill all of its vacancies, let's see if it could have gotten something better:
        ordered_lists_ranking_of_all_students_who_proposed=sort(lists_ranking_of_all_students_who_proposed[[(j-nStudents)]])
        if(ordered_lists_ranking_of_all_students_who_proposed[nSlots[(j-nStudents)]]<ranking_matched_students[nSlots[(j-nStudents)]]){#if the student could have gotten something better by rendering itself unacceptable to some student:
          benefits_from_manipulating_attractiveness[(j-nStudents)]=1
        }
      }
    }
  }
  proportion_benefits_misreporting=sum(benefits_from_manipulating_attractiveness/length(benefits_from_manipulating_attractiveness))
  return(list(final_match_function,proportion_benefits_misreporting,lists_ranking_of_all_students_who_proposed))
}
#=============================================
#
#=============================================





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
c.prefs_legal <- matrix(c(1,4,2,3,5,
                          5,2,3,4,1,
                          1,2,3,4,5),
                        byrow = FALSE, ncol = 3, nrow = 5)
nSlots <- c(2,1,1)

## SOSM mechanisms:
iaa(s.prefs = s.prefs, c.prefs = c.prefs, nSlots = nSlots,k=3, acceptance='deferred')

## Boston mechanisms:
iaa(s.prefs = s.prefs, c.prefs = c.prefs, nSlots = nSlots,k=3, acceptance='immediate')

#===========================================


