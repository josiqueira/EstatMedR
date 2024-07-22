data(diagnoses, package = "irr")
print(head(diagnoses))
cat("\n...\n")
print(tail(diagnoses))
cat("\n")
print(irr::kappam.fleiss(diagnoses, detail=TRUE))  # Fleiss' and category-wise Kappa

