# Rule to run the R targets pipeline
run_R:
	@echo "Running R analysis..."
	cd R && Rscript -e "source('main.R')"

slurm_R:
	@echo "Running analysis via slurm..."
	rm -f analysis_*.log
	rm -f analysis_*.stderr
	sbatch analysis.slurm
