!CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
!
        SUBROUTINE OLIVINE(T, P, Cp_ol, ro_ol, cond_ol)
!     ================
!
!***********************************************************************
!   20 June 2012: Created subroutine to calculate thermal conductivity for olivine phase.
!                        Olivine (Fo90): 3 orientations [010][001][100]
!***********************************************************************
!CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
!	
	IMPLICIT double PRECISION (a-h,o-z)
	DOUBLE PRECISION L(3) !conductivity vector
	INCLUDE 'Parameters_cond.txt'
!---------------------------------------------------------------------
!	IN THE 010 DIRECTION
!	Calculating Diffusivity (P, T). T (ºC), P (GPa)
	DiffT_ol_010=(ol_010_a)+(ol_010_b)*(exp(-(T/ol_010_c)))+
     &	(ol_010_d)*(exp(-(T/ol_010_e)))   
	DiffPT_ol_010=(DiffT_ol_010/(10**6))*(1+(5.00d-2*P))   
!	Calculating lattice conductivity;
	c_ol_010=(DiffPT_ol_010)*(ro_ol)*(Cp_ol*(1/pm_ol))
!	WRITE(*,*) 'Thermal conductivity=', c_ol_010
!	IN 001 DIRECTION:
!	Calculating Diffusivity (P, T). T (ºC), P (GPa)
	DiffT_ol_001=(ol_001_a)+(ol_001_b)*(exp(-(T/ol_001_c)))+
     &	(ol_001_d)*(exp(-(T/ol_001_e)))
	DiffPT_ol_001=(DiffT_ol_001/(10**6))*(1+(5.00d-2*P))
!	Calculating lattice conductivity;
	c_ol_001=(DiffPT_ol_001)*(ro_ol)*(Cp_ol*(1/pm_ol))
!	WRITE(*,*) 'Thermal conductivity=', c_ol_001
!	IN 100 DIRECTION:
!	Calculating Diffusivity (P, T). T (ºC), P (GPa)
	DiffT_ol_100=(ol_100_a)+(ol_100_b)*(exp(-(T/ol_100_c)))
     &	+(ol_100_d)*(exp(-(T/ol_100_e)))
	DiffPT_ol_100=(DiffT_ol_100/(10**6))*(1+(5.00d-2*P))
!	Calculating lattice conductivity:
	c_ol_100=(DiffPT_ol_100)*(ro_ol)*(Cp_ol*(1/pm_ol))
!	WRITE(*,*) 'Thermal conductivity=', c_ol_100
!	
!	Add radiative contribution:
	diam=1.0D0 !  grain size in cm
	Fe=0.1D0 !Fe-content: 0.1 of mass fraction
!     
!	The following are the same parameters used by Chris
!	in the model taconden from Hofmeister 2005. 
	alfa=1.8D0*(1.D0-(exp((-diam**1.3D0)/0.15D0)))-
     &	(1.D0-(exp((-diam**0.5D0)/5.D0)))
	beta=11.7D0*(exp(-diam/0.159))+6.D0*(exp((-diam**3.D0)/10.D0))
	xalfa=490.D0+1850.D0*(exp((-diam**0.315D0)/0.825D0))+
     &	875.D0*(exp(-diam/0.18D0))
	xbeta=2700.D0+9000.D0*(exp((-diam**0.5D0)/0.205D0))
	xwalfa=167.5D0+505.D0*(exp((-diam**0.5D0)/0.85D0))
	xwbeta=465.D0+1700.D0*(exp((-diam**0.94D0)/0.175D0))
!
	rad=(alfa*(exp(-((T-xalfa)**2.D0)/(2.D0*((xwalfa)**2.D0))))+
     & beta*(exp(-((T-xbeta)**2.D0)/(2.D0*((xwbeta)**2.D0)))))*(0.1/Fe)
!	
!	
	L=(/((c_ol_010)+rad),((c_ol_001)+rad),((c_ol_100)+rad)/)   	
        cond_ol=(L(1) + L(2) + L(3))/3.D0
!	write (1, *) T, P, Cp_ol, ro_ol, L(1), L(2), L(3), cond_ol,
!     &	DiffT_ol_010,
!     & DiffT_ol_001, DiffT_ol_100, DiffPT_ol_010, DiffPT_ol_001,
!     & DiffPT_ol_100, c_ol_010, c_ol_001, c_ol_100
!	
	RETURN
	END SUBROUTINE
!