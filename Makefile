.PHONY: clean

clean:
	rm -f *.html
	rm -rf figures
	rm -rf .created-dirs

.created-dirs:
	mkdir -p figures
	touch .created-dirs
	
figures/age_bar.png: .created-dirs Data/risk_factors.csv age_bar.R 
	Rscript age_bar.R

figures/no_sex_partners_bar.png: .created-dirs Data/risk_factors.csv no_sex_partner_bar.R
	Rscript no_sex_partner_bar.R

figures/preg_bar.png: .created-dirs Data/risk_factors.csv preg_bar.R 
	Rscript preg_bar.R

figures/smoke_hc_iud_bar.png: .created-dirs Data/risk_factors.csv smoke_hc_iud_bar.R 
	Rscript smoke_hc_iud_bar.R 

figures/smoke_covs_scatter.png: .created-dirs Data/risk_factors.csv smoke_covs_scatterplot.R 
	Rscript smoke_covs_scatterplot.R 

figures/hc_iud_dens_hist.png: .created-dirs Data/risk_factors.csv hc_iud_dens_hist.R 
	Rscript hc_iud_dens_hist.R 

figures/std_no_hist.png: .created-dirs Data/risk_factors.csv std_no_hist.R 
	Rscript std_no_hist.R

figures/disease_table.png: .created-dirs Data/risk_factors.csv disease_table.R
	Rscript disease_table.R

figures/first_last_diag_hist.png: .created-dirs Data/risk_factors.csv first_last_diag_hist.R
	Rscript first_last_diag_hist.R

figures/hor_hist_non_std.png: .created-dirs Data/risk_factors.csv  hor_bar_non_std.R 
	Rscript hor_bar_non_std.R

figures/cerv_cancer_result_hist.png: .created-dirs Data/risk_factors.csv cerv_cancer_pos_result_hist.R 
	Rscript cerv_cancer_pos_result_hist.R

figures/hor_bar_pos_test: .created-dirs Data/risk_factors.csv hor_bar_pos_test.R 
	Rscript hor_bar_pos_test.R

figures/corr_pos_test: .created-dirs Data/risk_factors.csv corr_pos_test.R 
	Rscript corr_pos_test.R

figures/age_cervical_cancer_density.png: .created-dirs Data/risk_factors.csv age_cervical_cancer_density.R 
	Rscript age_cervical_cancer_density.R

figures/partner_cervical_density.png: .created-dirs Data/risk_factors.csv partner_cervical_density.R 
	Rscript partner_cervical_density.R

figures/sex_time_cervical_cancer_density.png: .created-dirs Data/risk_factors.csv sex_time_cervical_density.R 
	Rscript sex_time_cervical_density.R

figures/smokes_cervical_density.png: .created-dirs Data/risk_factors.csv smokes_cervical_density.R
	Rscript smokes_cervical_density.R

figures/smokes_years_density.png: .created-dirs Data/risk_factors.csv smokes_years_density.R 
	Rscript smokes_years_density.R

figures/smokes_packs_year_density.png: .created-dirs Data/risk_factors.csv smokes_packs_year_density.R 
	Rscript smokes_packs_year_density.R

figures/hc_cervical_cancer_density.png: .created-dirs Data/risk_factors.csv hc_cervical_cancer_density.R 
	Rscript hc_cervical_cancer_density.R

figures/idc_cervical_cancer_density.png: .created-dirs Data/risk_factors.csv idc_cervical_cancer_density.R 
	Rscript idc_cervical_cancer_density.R

figures/corr_std_cervical.png: .created-dirs Data/risk_factors.csv corr_std_cervical.R 
	Rscript corr_std_cervical.R

figures/corr_dx_cervical.png: .created-dirs Data/risk_factors.csv corr_dx_cervical.R 
	Rscript corr_dx_cervical.R

figures/feature_selection1.png: .created-dirs Data/risk_factors.csv feature_selection1.R 
	Rscript feature_selection1.R


figures/feature_selection2.png: .created-dirs Data/risk_factors.csv feature_selection2.R 
	Rscript feature_selection2.R


figures/feature_selection3.png: .created-dirs Data/risk_factors.csv feature_selection3.R 
	Rscript feature_selection3.R



figures/dend_whole_pop.png: .created-dirs Data/behavorial_risk.csv dend_whole_pop_d2.R 
	Rscript dend_whole_pop_d2.R

figures/gap_stat_cluster_plot.png: .created-dirs Data/behavorial_risk.csv gap_stat_cluster_plot.R 
	Rscript gap_stat_cluster_plot.R


figures/k_means_cluster_plot.png: .created-dirs Data/behavorial_risk.csv k_means_cluster_plot.R 
	Rscript k_means_cluster_plot.R


figures/cluster_by_cervical_pca.png: .created-dirs Data/behavorial_risk.csv cluster_by_cervical_pca.R 
	Rscript cluster_by_cervical_pca.R


figures/variables_correlation_plot.png: .created-dirs Data/behavorial_risk.csv vars_corr_plot.R 
	Rscript vars_corr_plot.R


figures/corr_plot.png: .created-dirs Data/behavorial_risk.csv corr_plot_d2.R 
	Rscript corr_plot_d2.R


figures/feature_selection_d2.png: .created-dirs Data/behavorial_risk.csv feature_selection_d2.R 
	Rscript feature_selection_d2.R

report.html: age_bar.R no_sex_partner_bar.R preg_bar.R smoke_hc_iud_bar.R smoke_covs_scatterplot.R \
						hc_iud_dens_hist.R disease_table.R first_last_diag_hist.R hor_bar_non_std.R cerv_cancer_pos_result_hist.R \
						hor_bar_pos_test.R corr_pos_test.R age_cervical_cancer_density.R partner_cervical_density.R \
						sex_time_cervical_density.R smokes_cervical_density.R smokes_years_density.R smokes_packs_year_density.R \
						hc_cervical_cancer_density.R idc_cervical_cancer_density.R corr_std_cervical.R corr_dx_cervical.R \
						feature_selection1.R feature_selection2.R feature_selection3.R dend_whole_pop_d2.R gap_stat_cluster_plot.R \
						k_means_cluster_plot.R cluster_by_cervical_pca.R vars_corr_plot.R corr_plot_d2.R feature_selection_d2.R std_no_hist.R
		Rscript std_no_hist.R
		Rscript k_means_cluster_plot.R
		Rscript age_bar.R
		Rscript no_sex_partner_bar.R 
		Rscript preg_bar.R 
		Rscript smoke_hc_iud_bar.R 
		Rscript smoke_covs_scatterplot.R 
		Rscript hc_iud_dens_hist.R 
		Rscript disease_table.R 
		Rscript first_last_diag_hist.R 
		Rscript hor_bar_non_std.R 
		Rscript cerv_cancer_pos_result_hist.R 
		Rscript hor_bar_pos_test.R 
		Rscript corr_pos_test.R 
		Rscript age_cervical_cancer_density.R 
		Rscript partner_cervical_density.R 
		Rscript sex_time_cervical_density.R 
		Rscript smokes_cervical_density.R 
		Rscript smokes_years_density.R 
		Rscript smokes_packs_year_density.R 
		Rscript hc_cervical_cancer_density.R 
		Rscript idc_cervical_cancer_density.R 
		Rscript corr_std_cervical.R 
		Rscript corr_dx_cervical.R 
		Rscript feature_selection1.R 
		Rscript feature_selection2.R 
		Rscript feature_selection3.R 
		Rscript dend_whole_pop_d2.R 
		Rscript gap_stat_cluster_plot.R 
		Rscript cluster_by_cervical_pca.R 
		Rscript vars_corr_plot.R 
		Rscript corr_plot_d2.R 
		Rscript feature_selection_d2.R
		Rscript -e "rmarkdown::render(\"Report.Rmd\", output_format = \"html_document\")"
