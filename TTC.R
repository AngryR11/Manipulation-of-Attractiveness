#Written by Gustavo Quinderé Saraiva

rm(list = ls())
#set.seed(123)  # For replicability
library(igraph)

  

# Load necessary libraries
library(parallel)#to run session in multiple cores




log_message <- function(msg) cat(sprintf("[%s] %s\n", Sys.time(), msg))


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







top_trading_cycles <- function(s.prefs, c.prefs, nColleges=ncol(c.prefs),nSlots, k_bar=nColleges) {
  
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
  
  final_output=finish(s.prefs,c.prefs,college_vacancies,final_match_function,lists_ranking_of_all_students_who_proposed,nSlots)
  
  return(final_output)
}



# To Sum up and format the output
finish <- function(s.prefs,c.prefs,college_vacancies,final_match_function,lists_ranking_of_all_students_who_proposed,nSlots){
  nColleges=ncol(c.prefs)
  nStudents=ncol(s.prefs)
  benefits_from_manipulating_attractiveness=rep(0,nColleges)  
  lists_ranking_of_accepted_students <- vector("list", nColleges)
  
  matching<-data.frame(final_match_function)
  
  
  
  
  colleges=1:ncol(c.prefs)+nStudents
  
  matching <- matching[matching$student_vector!= 0, ]
  
  for(j in colleges){
    if(j %in% matching$student_match){
      ranking_matched_students=sort(which(c.prefs[,(j-nStudents)]%in%matching$student_vector[matching$student_match==j]))
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




# Example Usage
# s.prefs <- matrix(c(2, 1, 3,
#                     1, 3, 2,
#                     1, 3, 2,
#                     3, 1, 2,
#                     1, 2, 3),
#                   ncol = 5, byrow = FALSE)
# 
# c.prefs <- matrix(c(3, 4, 2, 3, 5,
#                     2, 5, 3, 4, 1,
#                     1, 2, 3, 4, 5),
#                   ncol = 3, byrow = FALSE)
# 
# nSlots <- c(1, 2, 1)
# k_bar <- 3
# 
# matching <- top_trading_cycles(s.prefs=s.prefs, c.prefs=c.prefs, nSlots=nSlots, k_bar=k_bar)
# print("Final Matching:")
# print(matching)

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

sample_size=150
m=400
q <- 5      #Each college has 5 slots
k<-5        #students have at most 5 acceptable choices
phi_grid=seq(1,10,.1)

theta_grid=seq(0,1,.2)

# Define a color palette for the lines
colors <- rainbow(length(theta_grid))

#matrix to store final results:
proportion_misreport=matrix(rep(rep(0,length(phi_grid)),length(theta_grid)),length(theta_grid),length(phi_grid))


# Function to run the simulation for a single phi value
run_simulation <- function(phi, theta, sample_size, m, q, k) {
  proportion_misreport <- 0
  for (s in 1:sample_size) {
    # Generate random preferences
    random_prefs <- generate_random_preferences(phi, m, q, theta)
    s.prefs <- t(random_prefs$s_prefs)
    c.prefs <- random_prefs$c_prefs
    nSlots <- random_prefs$n_slots
    
    # Run the top trading cycles algorithm
    result <- top_trading_cycles(s.prefs, c.prefs, nColleges = ncol(c.prefs), nSlots, k_bar = k)
    proportion_misreport <- proportion_misreport + result[[2]]  # Collect proportion for this sample
     }
  
  # Average the results for this phi
  return(proportion_misreport/sample_size)
}



# Use all available cores
num_cores <- detectCores() - 1
cl <- makeCluster(num_cores)


# Load necessary libraries on each worker
clusterEvalQ(cl, {
  library(igraph)  # Add all necessary libraries here
})




# Export necessary functions and variables to the cluster
clusterExport(cl, c(
  "generate_random_preferences", "top_trading_cycles", "find_cycles", 
  "finish", "m", "q", "k", "sample_size", "run_simulation","log_message"
))


#proportion_misreport=rep(0,length(phi_grid))
for(l in 1:length(theta_grid)){
  print(paste('theta:', l,sep=' '))
  theta=theta_grid[l]
  
  
  # Export theta to the cluster
  clusterExport(cl, c("theta"))
  
  # Run parallel computation for all phi in the grid
  phi_results <- parLapply(cl, phi_grid, function(phi) {
    run_simulation(phi, theta, sample_size, m, q, k)
  })
  
  
  
  
  # Store results
  proportion_misreport[l, ] <- unlist(phi_results)
}


#Save results in csv (in case one wants to format the layout of the plot later)
write.csv(proportion_misreport, file='proportion_misreport_TTC.csv',row.names=theta_grid)   


#Plotting results:
for(l in 1:length(theta_grid)){
  if(l==1){
    pdf('incentives_manipulate_TTC.pdf',width=7,height=5)
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




