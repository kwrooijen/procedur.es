module Fields (..) where

sections = create .sections  (\f r -> { r | sections = f r.sections })
procedures = create .procedures  (\f r -> { r | procedures = f r.procedures })
