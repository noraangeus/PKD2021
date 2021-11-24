-- {- updatePersonName (String, String, String, Integer, String) Int String
     -- Updates a datatabase with name and adress information about people. Specifically, changes the first or last name.
     --PRE: Input must follow argument type specification.
     --RETURNS: A tuple containing updated name and adress info.
     --EXAMPLES: updatePersonName  ("Angeus", "Nora", "Radmansgatan", "75432", "Osthammar") 1 "Lexie" -> ("Angeus","Lexie","Radmansgatan","75432","Osthammar")
     -- updatePersonName  ("Angeus", "Nora", "Radmansgatan", "75432", "Osthammar") 3 "Lexie" -> ("Angeus","Nora","Radmansgatan","75432","Osthammar")
--  -}
  
updatePersonName :: (String, String, String, Integer, String) -> Int -> String -> (String, String, String, Integer, String)
updatePersonName (fn, _, sa, pn, pa) 1 z = (fn, z, sa, pn, pa)
updatePersonName (_, gn, sa, pn, pa) 2 z = (z, gn, sa, pn, pa)
updatePersonName tuple _ _ = tuple
