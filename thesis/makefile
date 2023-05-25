vpath ./img/ ./data/ ./table/
DATA_FILES := ./data/GEDEvent_v22_1.csv ./data/ucdp-prio-acd-221.csv
DATA := ./data/weekly_data.RData ./data/res_data.RData ./data/monthly_data.RData
PARAMS := ./data/params_nb.RData ./data/params_ln.RData 
MONTHLY_PARAMS := ./data/monthly_params_nb.RData ./data/monthly_params_ln.RData 
PLOTS := ./img/weekly_deaths.pdf ./img/residuals.pdf ./img/PIT_plot.pdf \
		 ./img/pit_explain.pdf ./img/index_plot.pdf ./img/p0_plot.pdf \
		 ./img/disp_plot.pdf ./img/res_orig.pdf ./img/pred_plot.pdf \
		 ./img/monthly_res_plot.pdf
TABLES := ./tables/scoring_summary.tex \
		  ./tables/GED_table.tex \
		  ./tables/hot_residuals.tex

thesis: $(PLOTS) $(TABLES) $(wildcard *.tex)
	tectonic template/uiobachelor.tex -p -o .

$(PLOTS): %.pdf: %.R $(DATA) $(PARAMS)
	$(@:.pdf=.R)

$(TABLES): %.tex: %.R $(DATA) $(PARAMS)
	$(@:.tex=.R)

$(PARAMS): ./data/estimate_parameters.R $(DATA)
	./data/estimate_parameters.R

$(MONTHLY_PARAMS): ./data/monthly_estimate_params.R $(DATA)
	./data/monthly_estimate_params.R

$(DATA): %.RData: %.R $(DATA_FILES)
	$(@:.RData=.R)


