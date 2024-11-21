!CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
!
        SUBROUTINE ORTENSTATITE(T, P, Cp_OE90, ro_OE90, cond_OE90)
!     ================

!
!***********************************************************************
!   20 June 2012: Created subroutine to calculate thermal conductivity for 
!			ORTHOENSTATITE phase.
!                        ORTHOENSTATITE Mg90:  3 orientations [010][001][100]
!***********************************************************************
!CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
!	
	IMPLICIT double PRECISION (a-h,o-z)
	DOUBLE PRECISION L(3) 
	INCLUDE 'Parameters_cond.txt'
!---------------------------------------------------------------------
!	IN THE 010 DIRECTION
!	Calculating Diffusivity (P, T). T (ºC), P (GPa)
	DiffT_OE90_010=(OE90_010_a)+(OE90_010_b)*
     &	(exp(-(T/OE90_010_c)))+(OE90_010_d)*(exp(-(T/OE90_010_e)))
	DiffPT_OE90_010=(DiffT_OE90_010/(10**6))*(1+(5.00d-2*P))
!	Calculating lattice conductivity
	c_OE90_010=(DiffPT_OE90_010)*(ro_OE90)*(Cp_OE90*(1/pm_oe90))
!	WRITE(*,*) 'Thermal conductivity=', c_OE90_010
!	IN THE 001 DIRECTION
!	Calculating Diffusivity (P, T). T (ºC), P (GPa)
	DiffT_OE90_001=(OE90_001_a)+(OE90_001_b)*
     &	(exp(-(T/OE90_001_c)))+(OE90_001_d)*(exp(-(T/OE90_001_e)))
	DiffPT_OE90_001=(DiffT_OE90_001/(10**6))*(1+(5.00d-2*P))
!	Calculating lattice conductivity
	c_OE90_001=(DiffPT_OE90_001)*(ro_OE90)*(Cp_OE90*(1/pm_oe90))
!	WRITE(*,*) 'Thermal conductivity=', c_OE90_001
!	IN THE 100 DIRECTION
!	Calculating Diffusivity (P, T). T (ºC), P (GPa)
	DiffT_OE90_100=(OE90_100_a)+(OE90_100_b)*
     &	(exp(-(T/OE90_100_c)))+(OE90_100_d)*(exp(-(T/OE90_100_e)))
	DiffPT_OE90_100=(DiffT_OE90_100/(10**6))*(1+(5.00d-2*P))
!	Calculating lattice conductivity
	c_OE90_100=(DiffPT_OE90_100)*(ro_OE90)*(Cp_OE90*(1/pm_oe90))
!	WRITE(*,*) 'Thermal conductivity=', c_OE90_100
!	
!	Add radiative contribution to lattice conductivity:
	diam=1.0D0 !  grain size in cm
	Fe=0.1D0 !Fe-content: 0.1 of mass fraction
!     
!	The following are the same parameters used by Chris
!	in the model taken from Hofmeister 2005. 
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
	L=(/(c_OE90_010+rad), (c_OE90_001+rad), (c_OE90_100+rad)/)   
!
	cond_OE90=(L(1) + L(2) + L(3))/3.D0  
!	write (5, *) T, P, Cp_OE90, ro_OE90, L(1), L(2), L(3), cond_OE90,
!     & DiffT_OE90_010, DiffT_OE90_001, DiffT_OE90_100, DiffPT_OE90_010,
!     & DiffPT_OE90_001, DiffPT_OE90_100, c_OE90_010, c_OE90_001,
!     & c_OE90_100
	RETURN
	END SUBROUTINE
	
