!CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
!
        SUBROUTINE SPINEL(T, P, Cp_spi, ro_spi, cond_spi)
!     ================
!
!***********************************************************************
!    20 June 2012: Created subroutine to calculate thermal conductivity for spinel phase
!                        2 different samples: pink (real) and synthetic 
!***********************************************************************
!CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
!	
	IMPLICIT double PRECISION (a-h,o-z)
	DOUBLE PRECISION L(2)
	INCLUDE 'Parameters_cond.txt'
!---------------------------------------------------------------------
!	SPINEL PINK
!	Calculating Diffusivity (P, T). T (ºC), P (GPa)
       	DiffT_spi_pk=(spi_pk_a)+(spi_pk_b)*(exp(-(T/spi_pk_c)))
     &	+(spi_pk_d)*(exp(-(T/(spi_pk_e))))
	DiffPT_spi_pk=(DiffT_spi_pk/(10**6))*(1+(5.00d-2*P))
!	 Calculating lattice conductivity:
	c_spi_pk=(DiffPT_spi_pk)*(ro_spi)*(Cp_spi*(1/pm_spi))
!	WRITE(*,*) 'Thermal conductivity=', c_spi_pk
!	SPINEL SYNTETIC 
!	Calculating Diffusivity (P, T). T (ºC), P (GPa)
	DiffT_spi_syn=(spi_syn_a)+(spi_syn_b)*(exp(-(T/spi_syn_c)))
     &	+(spi_syn_d)*(exp(-(T/spi_syn_e)))
	DiffPT_spi_syn=(DiffT_spi_syn/(10**6))*(1+(5.00d-2*P))
!	Calcultaing lattice conductivity:
	c_spi_syn=(DiffPT_spi_syn)*(ro_spi)*(Cp_spi*(1/pm_spi))
!	WRITE(*,*) 'Thermal conductivity=', c_spi_syn
!
!	Add radiative contribution to lattice conductivity:
	diam=1.0D0 !  grain size in cm
	Fe=0.1D0 !Fe-content: 0.1 of mass fraction
!   
!	The following are the same parameters used
!	by Chris in the model taken from Hofmeister 2005. 
	alfa=1.8D0*(1.D0-(exp((-diam**1.3D0)/0.15D0)))-
     &	(1.D0-(exp((-diam**0.5D0)/5.D0)))
	beta=11.7D0*(exp(-diam/0.159))+6.D0*(exp(-(diam**3.D0)/10.D0))
	xalfa=490.D0+1850.D0*(exp((-diam**0.315D0)/0.825D0))+
     &	875.D0*(exp(-diam/0.18D0))
	xbeta=2700.D0+9000.D0*(exp((-diam**0.5D0)/0.205D0))
	xwalfa=167.5D0+505.D0*(exp((-diam**0.5D0)/0.85D0))
	xwbeta=465.D0+1700.D0*(exp((-diam**0.94D0)/0.175D0))
!
	rad=(alfa*(exp(-((T-xalfa)**2.D0)/(2.D0*((xwalfa)**2.D0))))+
     & beta*(exp(-((T-xbeta)**2.D0)/(2.D0*((xwbeta)**2.D0)))))*(0.1/Fe)
!	
	L=(/((c_spi_pk)+rad), ((c_spi_syn)+rad)/)
	cond_spi=(L(1) + L(2))/2.D0
!	write (2,*) T, P, Cp_spi, ro_spi, L(1), L(2), cond_spi,
!     &	DiffT_spi_pk, DiffT_spi_syn, DiffPT_spi_pk,
!     & DiffPT_spi_syn, c_spi_pk, c_spi_syn
!	
	RETURN
	END SUBROUTINE
 