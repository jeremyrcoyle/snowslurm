PKGNAME := snowslurm

buikd:
	R CMD build snowslurm

check:
	R CMD check snowslurm

clean:
	rm -rf ${PKGNAME}_*.tar.gz  ${PKGNAME}.Rcheck
