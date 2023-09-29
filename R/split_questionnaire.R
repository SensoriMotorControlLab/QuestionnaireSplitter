
library('jsonlite')

loadStructureJSON <- function(jsonfile) {
  
  structure <- jsonlite::fromJSON(jsonfile)
  
  return(structure)
  
}

extractComponentVariables <- function(structure, component) {
  
  for (session in c(1:length(structure$sessions))) {
    session_struct <- structure$sessions[[sprintf('%d',session)]]
    for (section in names(session_struct)) {
      section_struct <- session_struct[[section]]
      print(c(session, section))
      print( names( section_struct ) )
    }
  }
  
}