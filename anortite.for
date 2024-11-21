!CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
!
        SUBROUTINE ANORTITE(T, P, Cp_an, ro_an, cond_an)
!     ================
!
!***********************************************************************
!   20 June 2012: Created subroutine to calculate thermal conductivity for 
!			ANORTITE phase.
!                        ANORTITE : only one sample
!***********************************************************************
!CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
!	
	IMPLICIT double PRECISION (a-h,o-z)
	INCLUDE 'Parameters_cond.txt'
!
!	Calculating Diffusivity (P, T). T (ºC), P (GPa)
       	DiffT_an=(an_a)+(an_b)*(exp(-(T/an_c)))+(an_d)*
     &	(exp(-(T/an_e)))
	DiffPT_an=(DiffT_an/(10**6))*(1+(5.00d-2*P))
!	Calculating lattice conductivity;
	c_an=(DiffPT_an)*(ro_an)*(Cp_an*(1/pm_an))
!	WRITE(*,*) 'Thermal conductivity=', c_an
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
	cond_an=(c_an+rad)
!
!	write (7, *) T, P, Cp_an, ro_an, cond_an, DiffT_an, DiffPT_an, c_an
	RETURN
	END SUBROUTINE
	