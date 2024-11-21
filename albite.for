!CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
!
        SUBROUTINE ALBITE(T, P, Cp_alb, ro_alb, cond_alb)
!     ================
!
!***********************************************************************
!   20 June 2012: Created subroutine to calculate thermal conductivity for ALBITE phase.
!                        ALBITE:  3 orientations [010][001][_|_]
!***********************************************************************
!CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
!	
	IMPLICIT double PRECISION (a-h,o-z)
	DOUBLE PRECISION L(3) 
	INCLUDE 'Parameters_cond.txt'
!---------------------------------------------------------------------	
!	IN 010 DIRECTION:
!	Calculating Diffusivity (P, T). T (ºC), P (GPa)
	DiffT_alb_010=(alb_010_a)+(alb_010_b)*
     &	(exp(-(T/alb_010_c)))+(alb_010_d)*(exp(-(T/alb_010_e)))
	DiffPT_alb_010=(DiffT_alb_010/(10**6))*(1+(5.00d-2*P))
!	Calculating lattice conductivity:
	c_alb_010=(DiffPT_alb_010)*(ro_alb)*(Cp_alb*(1/pm_alb))
!	WRITE(*,*) 'Thermal conductivity=', c_alb_010
!	IN 001 DIRECTION:
!	Calculating Diffusivity (P, T). T (ºC), P (GPa)
	DiffT_alb_001=(alb_001_a)+(alb_001_b)*
     &	(exp(-(T/alb_001_c)))+(alb_001_d)*(exp(-(T/alb_001_e)))
	DiffPT_alb_001=(DiffT_alb_001/(10**6))*(1+(5.00d-2*P))
!	Calculating lattice conductivity:
	c_alb_001=(DiffPT_alb_001)*(ro_alb)*(Cp_alb*(1/pm_alb))
!	IN _!_ DIRECTION:
!	Calculating Diffusivity (P, T). T (ºC), P (GPa)
!	WRITE(*,*) 'Thermal conductivity=', c_alb_001
	DiffT_alb_p=(alb_p_a)+(alb_p_b)*
     &	(exp(-(T/alb_p_c)))+(alb_p_d)*(exp(-(T/alb_p_e)))
	DiffPT_alb_p=(DiffT_alb_p/(10**6))*(1+(5.00d-2*P))
!	Calculating lattice conductivity:
	c_alb_p=(DiffPT_alb_p)*(ro_alb)*(Cp_alb*(1/pm_alb))
!	WRITE(*,*) 'Thermal conductivity=', c_alb_p
!
!	Add radiative contribution :
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
	L=(/(c_alb_010+rad), (c_alb_001+rad),(c_alb_p+rad)/) 
!
	cond_alb=(L(1) + L(2) + L(3))/3.D0
!	write (8, *) T, P, Cp_alb, ro_alb, L(1), L(2), L(3), cond_alb,
!     & DiffT_alb_010, DiffT_alb_001, DiffT_alb_p, DiffPT_alb_010, 
!     & DiffPT_alb_001, DiffPT_alb_p, c_alb_010, c_alb_001, c_alb_p
!	
	RETURN
	END SUBROUTINE
	