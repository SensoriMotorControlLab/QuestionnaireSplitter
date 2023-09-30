
library('jsonlite')

loadStructureJSON <- function(jsonfile) {
  
  structure <- jsonlite::fromJSON(jsonfile)
  
  return(structure)
  
}

extractComponentVariables <- function(structure, component) {
  
  component_hits <- list()
  
  for (session in c(1:length(structure$sessions))) {
    session_struct <- structure$sessions[[sprintf('%d',session)]]
    for (section in names(session_struct)) {
      section_struct <- session_struct[[section]]
      if (component %in% names( section_struct) ) {
        print(c(session, section))
        column_details <- section_struct[[component]]
        component_hits[[length(component_hits)+1]] <- list('session'=session,
                                                           'section'=section,
                                                           'column_details'=column_details)
      }
    }
  }
  
  return(component_hits)
  
}

rawQuestionnaireReader <- function(year, semester, session) {
  
  filename <- sprintf('../raw/%s_%s_session%s.csv', year, semester, session)
  
  first_line = readLines(filename, n=1)
  
  first_line <- strsplit(first_line, ',')
  
  all_df <- read.csv(filename, skip=1, strip.white = FALSE)
  
  second_line <- names(all_df)
  
  all_df <- all_df[-1,]
  
  return( list('column_code'=first_line,
               'question_text'=second_line,
               'data'=all_df) )
  
}