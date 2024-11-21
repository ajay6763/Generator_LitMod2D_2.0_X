!CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
!
        SUBROUTINE GARNET(T, P, Cp_gt, ro_gt, cond_gt)
!     ================
!
!***********************************************************************
!   20 June 2012: Created subroutine to calculate thermal conductivity for GARNET phase.
!                        GARNET: 2 compositions (Py-Al, Ant-Hill)
!***********************************************************************
!CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
!	
	IMPLICIT double PRECISION (a-h,o-z)
	DOUBLE PRECISION L(2) 
	INCLUDE 'Parameters_cond.txt'
!---------------------------------------------------------------------
!	Py-AL GRANATE:
!	Calculating Diffusivity (P, T). T (ºC), P (GPa)
	DiffT_gt_PyAl=gt_PyAl_a+(gt_PyAl_b)*
     &	(exp(-(T/gt_PyAl_c)))+(gt_PyAl_d)*(exp(-(T/gt_PyAl_e)))
	DiffPT_gt_PyAl=(DiffT_gt_PyAl/(10**6))*(1+(5.00d-2*P))
!	Calculating lattice conductivity
	c_gt_PyAl=(DiffPT_gt_PyAl)*(ro_gt)*(Cp_gt*(1/pm_gt))
!	WRITE (*,*) 'Thermal conductivity=', c_gt_PyAl
!	SYNTHETIC GRANATE
!	Calculating Diffusivity (P, T). T (ºC), P (GPa)
	DiffT_gt_AntH=gt_AntH_a+(gt_AntH_b)*
     &	(exp(-(T/gt_AntH_c)))+(gt_AntH_d)*(exp(-(T/gt_AntH_e)))
	DiffPT_gt_AntH=(DiffT_gt_AntH/(10**6))*(1+(5.00d-2*P))
!	Calculating lattice conductivity
	c_gt_AntH=(DiffPT_gt_AntH)*(ro_gt)*(Cp_gt*(1/pm_gt))
!	WRITE (*,*) 'Thermal conductivity=', c_gt_AntH
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
	L=(/(c_gt_PyAl+rad), (c_gt_AntH+rad)/) 
	cond_gt=(L(1) + L(2))/2.D0
!	
!	WRITE (9,*) T, P, Cp_gt, ro_gt, L(1), L(2), cond_gt,
!     & DiffT_gt_PyAl, DiffT_gt_AntH, DiffPT_gt_PyAl, DiffPT_gt_AntH,
!     & c_gt_PyAl, c_gt_AntH     
	RETURN
	END SUBROUTINE
	