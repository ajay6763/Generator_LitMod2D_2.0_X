!CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
!
        SUBROUTINE DIOPSIDE(T, P, Cp_di, ro_di, cond_di)
!     ================
!
!***********************************************************************
!   20 June 2012: Created subroutine to calculate thermal conductivity for
!			DIOPSIDE phase.
!                        Diopside: 3 orientations [110][001][100]
!***********************************************************************
!CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
!	
	IMPLICIT double PRECISION (a-h,o-z)
	DOUBLE PRECISION L(3) 
	INCLUDE 'Parameters_cond.txt'
	
	P=P*1.d-4
!---------------------------------------------------------------------
!	IN THE 010 DIRECTION
!	Calculating Diffusivity (P, T). T (ºC), P (GPa)
	DiffT_di_110=di_110_a+(di_110_b)*
     & (exp(-(T/di_110_c)))+(di_110_d)*(exp(-(T/di_110_e)))
	DiffPT_di_110=(DiffT_di_110/(10**6))*(1+(5.00d-2*P))
!	Calculating lattice conductivity:
	c_di_110=(DiffPT_di_110)*(ro_di)*(Cp_di*(1/pm_di))
!	WRITE (*,*) 'Thermal conductivity=', c_di_110
!	IN THE 110 DIRECTION
!	Calculating Diffusivity (P, T). T (ºC), P (GPa)
        DiffT_di_001=di_001_a+(di_001_b)*
     & (exp(-(T/di_001_c)))+(di_001_d)*(exp(-(T/di_001_e))) 
	DiffPT_di_001=(DiffT_di_001/(10**6))*(1+(5.00d-2*P))
!	Calculating lattice conductivity:
	c_di_001=(DiffPT_di_001)*(ro_di)*(Cp_di*(1/pm_di))
!	WRITE (*,*) 'Thermal conductivity=', c_di_001
!	IN THE 100 DIRECTION
!	Calculating Diffusivity (P, T). T (ºC), P (GPa)
	DiffT_di_100=di_100_a+(di_100_b)*
     &	(exp(-(T/di_100_c)))+(di_100_d)*(exp(-(T/di_100_e)))
	DiffPT_di_100=(DiffT_di_100/(10**6))*(1+(5.00d-2*P))
!	Calculating lattice conductivity:
	c_di_100=(DiffPT_di_100)*(ro_di)*(Cp_di*(1/pm_di))
!	WRITE (*,*) 'Thermal conductivity=', c_di_100
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
	L=(/(c_di_110+rad), (c_di_001+rad), (c_di_100+rad)/)   
!	
	cond_di=(L(1) + L(2) + L(3))/3.D0 
!	write (3, *) T, P, Cp_di, ro_di, L(1), L(2), L(3), cond_di, 
!     & DiffT_di_110, DiffT_di_001, DiffT_di_100, DiffPT_di_110,
!     & DiffPT_di_001, DiffPT_di_100, c_di_110, c_di_001, c_di_100
	RETURN
	END SUBROUTINE
	
