library(shiny) #import shiny library for GUI
college = read.csv("college.csv") #read in data file
accept = lm(formula = Percent.admitted ~ Average.GPA + Average.SAT.Score, data = college) #run regression to calculate acceptance rate for student, which we will use later

costuserPref = function(a,b) { #function to calculate score based off of user inputs for cost
  diff = a - b
  x = ifelse(diff>=0, 100, 0)
  return(x)
}




acaduserPref = function(d, e) { #function to calculate academic score based off  standard error of OLS regression model of 11.7
  acad = e - d
  schoolfit = ifelse(acad >= 2*11.7, 100,  ifelse(acad >= 1.5*11.7, 95, ifelse(acad >= 11.7, 90, ifelse(acad >= .5*11.7, 80, ifelse(acad>=0, 70, ifelse(acad >= .5*-11.7, 50, ifelse(acad >= -11.7, 40, ifelse(acad>= 1.5*-11.7, 20, ifelse(acad>=2*-11.7, 10, 0)))))))))
  return(schoolfit)
  
}

aidUserPref = function(f) {
  schollies = (f - min(f))/(max(f) - min (f)) * 100 #normalize the scholarship ranking by finding the max and min in our dataset and calculating a score
  return(schollies)
}

shinyServer(function(input, output) {
  preferences = reactive ({     #create a reactive GUI function that incorporates all of user inputs and calls functions above to calcualte a total score for each college
    Average.GPA = input$userGPA
    Average.SAT.Score = input$userSAT
    studentdata = data.frame(Average.GPA,Average.SAT.Score)
    prediction = predict(accept, studentdata) #predicted acceptance rate based off of regression model of GPA and SAT on college acceptance
    if (input$Outstate == TRUE) { #calculate score if student paying out of state tuition
      score = costuserPref(input$FundsAvailable, college$Out.of.state.tuition) * (input$Cost/100) + acaduserPref(prediction, college$Percent.admitted) * (input$Academics/100) + aidUserPref(college$Percent.of.students.receiving.scholarships) * (input$Scholarship/100)  #calculate score by calling functions for academics, cost, and scholarships
    }
    else{  #otherwise student pays in-state tuition   
      score = costuserPref(input$FundsAvailable, college$In.state.tuition) * (input$Cost/100) + acaduserPref(prediction, college$Percent.admitted) * (input$Academics/100) + aidUserPref(college$Percent.of.students.receiving.scholarships) * (input$Scholarship/100)
    }
    score = round(score, digits = 0)
    return(score)
  })
  output$school = renderDataTable({
    WeightedScore = preferences() #call the preferences function to retrieve personalized score for each school
    college = cbind(WeightedScore, college) #append the weightedscore to the college table, which is diplayed by the app
    college = college[order(-WeightedScore),] #sort data in descending order
    college = college[college$Total.enrollment >= input$Enrollment[1] & college$Total.enrollment <= input$Enrollment[2],]   #filter data based off of user inputs for the slider total enrollment
    if (input$Type != 'All') {
      college = college[college$Institution.Type == input$Type,]  #filter colleges by type (public/private) type if 'Any' is not selected
    }
    if (input$State != 'USA') {
      college = college[college$State == input$State,]  #filter colleges dataset by state if 'USA' is not selected 
    }
    if (input$Campus != 'Any') {
      college = college[college$Campus == input$Campus,] #filter by campus type
    }
    # rename columns
    colnames(college)[1] = "Score"
    colnames(college)[2] = "College"
    colnames(college)[3] = "Web Address"
    colnames(college)[6] = "Type of Institution"
    colnames(college)[7] = "Campus Type"
    colnames(college)[8] = "Enrollment"
    colnames(college)[9] = "In-State Tuition"
    colnames(college)[10] = "Out-of-State Tuition"
    colnames(college)[11] = "Acceptance Rate"
    colnames(college)[12] = "Average GPA"
    colnames(college)[13] = "Average SAT"
    colnames(college)[14] = "Average ACT"
    colnames(college)[15] = "% Receiving Scholarships"
    colnames(college)[16] = "Retention Rate"
    colnames(college)[17] = "Graduation Rate"
    colnames(college)[18] = "% Non-White"
    college
  }
  
  
  )
  
  
  output$value <- renderUI({    #display instructions to user and adapt to user inputs
    if (input$Academics + input$Cost + input$Scholarship > 100) {
      h5('The sum of the weights must equal 100  You are currently over 100!  Please fix.  You will get an inaccurate result!')
    }
    else if (input$Academics + input$Cost + input$Scholarship < 100) {
      print('Please enter your weights for the categories and enter your corresponding data for the categories.  Ensure that weights sum up to 100 or you will not be able to proceed')
    }
    else {
      print('You have entered sufficient information for the weights.  Check the values for your inputs and click on the "College Database" tab.')
      
    }

})})
  




