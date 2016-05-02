#############################################################################################
# Description of the datasets one can load from the main script. 
#
# Vincent Labatut 04/2016
#############################################################################################



data.pars <- list()
# The name in the list is later used to retrieve the needed files.
# Each element of the list describe a given dataset with the following fields:
# - data.folder represents the folder containing all these files.
# - rdata.filename is the name of the R data file containing the igraph objects.
# - centrality.filenam is the file generated with MuxViz and containing reference centrality values, to be compared with opinion centrality.

data.pars[["Arabidopsis"]] <- list(
	data.folder="Arabidopsis_Multiplex_Genetic/",
	rdata.filename="Arabidopsis.Rdata",
	centrality.filename="Arabidopsis_centrality_table.csv")

data.pars[["Celegans"]] <- list(
	data.folder="Celegans_Multiplex_Genetic/",
	rdata.filename="Celegans.Rdata",
	centrality.filename="Celegans_centrality_table.csv")

data.pars[["CKM"]] <- list(
	data.folder="CKM-Physicians-Innovation_Multiplex_Social/",
	rdata.filename="CKMPI.Rdata",
	centrality.filename="CKM_centrality_table.csv")

data.pars[["CS_Aarhus"]] <- list(
	data.folder="CS-Aarhus_Multiplex_Social/",
	rdata.filename="CSAarhus.Rdata",
	centrality.filename="CS_Aarhus_centrality_table.csv")

data.pars[["Drosophila"]] <- list(
	data.folder="Drosophila_Multiplex_Genetic/",
	rdata.filename="Drosophila.Rdata",
	centrality.filename="Drosophila_centrality_table.csv")

data.pars[["EUAir"]] <- list(
	data.folder="EUAir_Multiplex_Transport/",
	rdata.filename="EUAir.Rdata",
	centrality.filename="EUAir_centrality_table.csv")

data.pars[["FAO"]] <- list(
	data.folder="FAO_Multiplex_Trade/",
	rdata.filename="FAO.Rdata",
	centrality.filename="FAO_centrality_table.csv")

data.pars[["HepatitusCVirus"]] <- list(
	data.folder="HepatitusCVirus_Multiplex_Genetic/",
	rdata.filename="HepatitusCVirus.Rdata",
	centrality.filename="HepatitusCVirus_centrality_table.csv")

data.pars[["HumanHIV1"]] <- list(
	data.folder="HumanHIV1_Multiplex_Genetic/",
	rdata.filename="HumanHIV1.Rdata",
	centrality.filename="HumanHIV1_centrality_table.csv")

data.pars[["Kapferer1"]] <- list(
	data.folder="kaptail1-GraphML/",
	rdata.filename="kaptail1.Rdata",
	centrality.filename="Kapferer1_centrality_table.csv")

data.pars[["Kapferer2"]] <- list(
	data.folder="kaptail2-GraphML/",
	rdata.filename="kaptail2.Rdata",
	centrality.filename="Kapferer2_centrality_table.csv")

data.pars[["Knoke"]] <- list(
	data.folder="knokbur-GraphML/",
	rdata.filename="knokbur.Rdata",
	centrality.filename="Knoke_centrality_table.csv")

data.pars[["Lazega"]] <- list(
	data.folder="Lazega-Law-Firm_Multiplex_Social/",
	rdata.filename="Lazega.Rdata",
	centrality.filename="Lazega_centrality_table.csv")

data.pars[["London"]] <- list(
	data.folder="London_Multiplex_Transport/",
	rdata.filename="London.Rdata",
	centrality.filename="London_centrality_table.csv")

data.pars[["Padgett"]] <- list(
	data.folder="padgett-GraphML/",
	rdata.filename="padgett.Rdata",
	centrality.filename="Padgett_centrality_table.csv")

data.pars[["PierreAuger"]] <- list(
	data.folder="PierreAuger_Multiplex_Coauthorship/",
	rdata.filename="PierreAuger.Rdata",
	centrality.filename="PierreAuger_centrality_table.csv")

data.pars[["Rattus"]] <- list(
	data.folder="Rattus_Multiplex_Genetic/",
	rdata.filename="Rattus.Rdata",
	centrality.filename="Rattus_centrality_table.csv")

data.pars[["Roethlisberger"]] <- list(
	data.folder="wiring-GraphML/",
	rdata.filename="wiring.Rdata",
	centrality.filename="Roethlisberger_centrality_table.csv")

data.pars[["Sampson"]] <- list(
	data.folder="sampson-GraphML/",
	rdata.filename="sampson.Rdata",
	centrality.filename="Sampson_centrality_table.csv")

data.pars[["Thurmann"]] <- list(
	data.folder="thuroff-GraphML/",
	rdata.filename="thuroff.Rdata",
	centrality.filename="Thurmann_centrality_table.csv")

data.pars[["Wolfe"]] <- list(
	data.folder="wolfe-GraphML/",
	rdata.filename="wolfe.Rdata",
	centrality.filename="Wolfe_centrality_table.csv")
