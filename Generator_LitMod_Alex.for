       program generator
c************************************************************************* 
c        Very simple interface to create the property tables for 
c        mantle bodies used in the LitMod codes
c  
C        GO TO LINE 111 TO CHANGE THE DEFAULT MAXIMUM PRESSURE AND
C        TEMPERATURE FOR THE CALCULATION OF PROPERTIES. 
c
c        This version implements a new calculator for thermal conductivity  
c        based on results by Anne Hofmeister and fitted by Chris Grose
c        Lavinia Tunini contributed some subroutines
c
c        this is a poorly written code...much could be improved.
c        modified 07/2012 JCA
c        modified 11/2013 MF
c           PREMIN changed to 1500bar for sublithospheric mantle.
c           Note that temmin and temmax for sublithospheric mantle are set
c           to 1173K and 2200K, respectively (case javi=1)
c	modified 03/23 by AR in order to run Perple_X 6.9.1 and later updates of perple_X
c
c TO COMPILE : gfortran Generator_LitMod_Alex.for olivine.for diopside.for garnet.for albite.for anortite.for ortenstatitemg90.for spinel.for generator_table_atten_corr.for -o  Generator_LitMod_Alex
 
c
C*************************************************************************
      IMPLICIT double PRECISION (a-h,o-z) 
      integer i,indice,char2,kkolor,kkolor2,javi,hh
	integer :: check
      character(100)datfil, path,fname,path1,datfil1,char
	character(10)premax,temmax,premin,temmin
	PARAMETER (ocsio2=45.5,ocal2o3=4.4,ocmgo=38.33,ocfeo=8.1,
     *           occao=3.55)
       
      REAL*8, DIMENSION(:), ALLOCATABLE :: R0,R1,R2,R3,R4,R5,R6,R7,R8,
     *                               R9,R10,R11,R12,R13,R14,R15,
     *                               R16,R17,R18,R19,R20,R21,R22,
     *                               R23,R24,R25,R26,R27,R28,R29,  
     *                               R30,R31,R32,R33,R34,R35,R36,R37
     
	CHARACTER(LEN=16), DIMENSION(:), ALLOCATABLE :: jca
	CHARACTER:: ifilem*2
C    ...Change here the maxium and minimum P (in bars) and T (in K)
c    ...premax is the max P; temmax is the max T., etc...
c	premax='0.11E6'
c	temmax='1500'
c	premin='50'
c	temmin='500'
c ... initialize some variables	
	aSi=0.0E0
	bAl=0.0E0
	cFe=0.0E0
	dMg=0.0E0
	eCa=0.0E0
	fNa=0.0E0
	gK=0.0E0
	h20=0.0E0
	hCr=0.0E0
	kkolor=0
	kkolor2=0
	javi=0
	indice=0
	char2=0

	      
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
     
c      write(*,*) 'Enter the pressure [Kbar] at which 
c     *            the composotion will vary'
c	read(*,*) press

c     ...we start a loop for different compositions
c           assigned to the system...

             
*********************************************
c  this part is only for one composition...
      write(*,*)'************************************************'
	write(*,*)'               HI THERE!!!!                     '
	write(*,*)''
	write(*,*)'  I WILL HELP YOU TO CREATE YOUR OWN CUSTOMIZED '
	write(*,*)'      MANTLE/CRUST BODIES TO USE IN LITMOD...   '
	write(*,*)''
      write(*,*)' #Note#: This is only a simple interface designed'
	write(*,*)'         to extract information used by LitMod   ' 
	write(*,*)'         from Perple_X (www.perplex.ethz.ch).    '
	write(*,*)'         Proper acknowledgement of this software '
	write(*,*)'         is then always required.                '
	write(*,*)''
	write(*,*)'                        ...Let us start!!       '
	write(*,*)'************************************************'
      write(*,*)''
      write(*,*)''
      write(*,*)'We will use the H&P98 thermodynamic database    '           '                                       '
	indice=5  	

	write(*,*) ''

call system('rm Buildfile.dat')
c     ...sublithospheric vs lithospheric mantle
      print *, 'Will this be a sublithospheric mantle?'
      print *, 'YES = 1            NO = 0'
	read(*,*) javi

	if(javi.eq.0)then
	premax='0.145E+06'
	temmax='1823.0'
	premin='10'
	temmin='273'
	elseif(javi.eq.1)then
	print *, ''
	print *, 'I will take longer in this case because the ' 
      print *, ' T range is larger... be patient an let me do the job!!'
      premax='0.145E+06'
	temmax='2200.0'
	premin='1500'
	temmin='1173'
	endif
      print *, ''


c   ...check for water, Na, and K
      if(indice.eq.5)then
	write(*,*)'Do you have H2O and K2O in your system?'
      write(*,*) 'YES = 1        NO = 0'
	read(*,*) kkolor
      endif

  
c... select the correct composition...
      write(*,*)'Now enter the bulk composition (in wt%) for the oxides'
      if(kkolor.eq.1)then
	 write(*,*)'SiO2 Al2O3 FeO MgO CaO Na2O K2O H2O (in this order,
     * one per line)'
	 write(*,*) ''
	read(*,*) aSi, bAl, cFe, dMg, eCa, fNa, gK, h20
	cctot=aSi+bAl+cFe+dMg+eCa+fNa+gK+h20

	else
       write(*,*)'SiO2 Al2O3 FeO MgO CaO Na2O (in this order,
     * one per line)'
	 write(*,*) ''
       read(*,*) aSi, bAl, cFe, dMg, eCa, fNa
       cctot=aSi+bAl+cFe+dMg+eCa+fNa
	endif

c    ...checking oxide amounts...
      if(cctot.ne.100.E0)then
      if(ABS(cctot-100.000E0).gt.0.000)then
	write(*,*) ''
	write(*,*)'**********************************************'
	write(*,*)'Note that these values do not add up to 100%'
	write(*,*)'The absolute error (in wt%) is',' =', ABS(cctot-100.00)
	write(*,*)'If you do not stop here, I will correct to 100% '
	write(*,*)'**********************************************'
      endif
      endif

c ... name of your mantle/folder	
	write(*,*) ''
	write(*,*) 'Name of the body (mantle/crust)'
	read *, char
        
c... which tables do you need?
	write(*,*) ''
	write(*,*) 'Do you need the FULL (prop. + system) table?  '
	write(*,*) 'For "Yes" type 1 ; For "NO" type 0'
	read *, char2
c	char2=1 !! A CHANGER ICI
     
     
**********************************************
	datfil="Buildfile"
**********************************************       
c   ...ignore this, but do not delet it!     
       goto 777

c      do 77 i=1,30 

c      datfil="attenuation.dat"
c      parmel=i

c    ...here you have to indicate how the composition
c       is going to vary with each iteration...
************************************************************************

c     ...estimating major element composition of the residue (formulation taken
c        from Niu (1997)...corrected!!!
    

c    ...Bulk distribution coefficients...
      DSiO2=( 0.848E0-(0.22E0*(parmel/100.0E0))+(0.0055*press) )
	if(parmel.le.0.2)then
	DAl2O3=0.08E0
	else
	DAl2O3=( 0.189E0-(0.51E0*(parmel/100.0E0))
     *      -(0.00025/(parmel/100.0E0))+(0.001E0*press) )
      endif

	if(DAl2O3.le.0.01E0)then
      DAl2O3=0.01E0
	else
	DAl2O3=DAl2O3
	endif
	      
	DMgO=( 5.2E0-(4.56641E0*(parmel/100.0E0))
     *    -(0.0594E0*press) )
	DFeO=( 0.316913E0+(0.3695E0*(parmel/100.0E0))
     *    -(0.003458E0*press)+(0.213*DMgO) )
	DCaO=(0.318E0-(1.22E0*(parmel/100.0E0)) 
     *    +(0.00272E0/(parmel/100.0E0))+(0.0005E0*press) )
      if(DCaO.le.1.7E0)then
	  if(DCaO.gt.0.005E0)then
        DCaO=DCaO
	  else
	  DCaO=0.005E0
	  endif
	else
	DCaO=1.7E0
	endif
    
c    ...composition of the solid residue...    
      xsSiO2=(ocsio2/( ((parmel/100.0E0)/DSiO2)
     *      +(1.0E0-(parmel/100.0E0)) ) )
      xsAl2O3=(ocal2o3/( ((parmel/100.0E0)/DAl2O3)
     *      +(1.0E0-(parmel/100.0E0)) ) )
      xsMgO=(ocmgo/( ((parmel/100.0E0)/DMgO)
     *      +(1.0E0-(parmel/100.0E0)) ) )
      xsFeO=(ocfeo/( ((parmel/100.0E0)/DFeO)
     *      +(1.0E0-(parmel/100.0E0)) ) )
      xsCaO=(occao/( ((parmel/100.0E0)/DCaO)
     *      +(1.0E0-(parmel/100.0E0)) ) )
    
      xstot=xsSiO2+xsAl2O3+xsMgO+xsFeO+xsCaO


	aMg=xsMgO
	bAl=xsAl2O3
	cSi=xsSiO2
	dCa=xsCaO
	eFe=xsFeO


777   write(*,*)''
      write(*,*) '...Working for you...'

c ...selecting the different databases/formalisms

c********************************************
c   ...writing the text file for build.exe ...
c    ...remember to change the file names here!!!!
c**************************************

c****************************************************************
c ...this is for new perplex dat file (new = 2022) ALEX...
c      datfil="Buildfile.dat"

6011  open(1,file="Buildfile.txt")
	if(kkolor.eq.1)then
	write(1,1110)datfil
	write(1,1011)
	write(1,1880)temmin,temmax,premin,premax
	write(1,2011) aSi,bAl,cFe,dMg,eCa,fNa,gK,h20
   	write(1,1012)
1110  format(A20)
1011  format("hp02ver.dat",/ 
     *        ,/  
     *        "n",/  
     *        "2",/  
     *        "n",/  
     *        "n",/  
     *        "n",/  
     *        "SIO2",/  
     *        "AL2O3",/  
     *        "FEO",/  
     *        "MGO",/  
     *        "CAO",/
     *        "NA2O",/ 
     *        "K2O",/ 
     *        "H2O",/    
     *         ,/ 
     *        "5",/  
     *        "n",/ 
     *        "2",/) 
1880  format(a14,1X,a14,/  
     *       a14,1X,a14,/  
     *        "y")    
2011  format(F7.3,7F7.3)    
1012  format( "y",/  
     *        "y",/  
     *        "n",/  
     *        "stv",/
     *        "zo",/
     *        "ab",/
     *        "foL",/
     *        "faL",/
     *        "fa8L",/
     *        "anL",/
     *        "enL",/
     *        "sil8L",/
     *        "fo8L",/
     *        "q8L",/
     *        "h2oL",/
     *        "nasGL",/     
     *        "abL",/
     *        "diL",/  
     *        ,/ 
     *        "y",/  
     *        "solution_model.dat",/      
     *        "O(HP)",/
     *        "Opx(HP)",/ 
     *        "Cpx(HP)",/ 
     *        "Sp(HP)",/ 
     *        "Pl(h)",/
     *        "Gt(stx8)",/
     *        "Wad(HP)",/
     *        "Atg(PN)",/
     *        "T",/
     *        ,/ 
     *        "calculationtitle",/)
	close (1)
	
                  elseif(kkolor.eq.0)then  

	write(1,1117)datfil
	write(1,1711)
	write(1,1887)temmin,temmax,premin,premax
	write(1,2511) aSi,bAl,cFe,dMg,eCa,fNa
   	write(1,1312)
1117  format(A20)
1711  format("hp02ver.dat",/ 
     *        ,/  
     *        "n",/  
     *        "2",/  
     *        "n",/  
     *        "n",/  
     *        "n",/  
     *        "SIO2",/  
     *        "AL2O3",/  
     *        "FEO",/  
     *        "MGO",/  
     *        "CAO",/
     *        "NA2O",/ 
     *         ,/ 
     *        "n",/ 
     *        "2",/) 
1887  format(a14,1X,a14,/  
     *       a14,1X,a14,/  
     *        "y")    
2511  format(F7.3,5F7.3)    
1312  format( "y",/  
     *        "y",/  
     *        "n",/
     *        "stv",/
     *        "zo",/
     *        "ab",/
     *        "fo8L",/
     *        "foL",/
     *        "faL",/
     *        "fa8L",/
     *        "anL",/
     *        "enL",/
     *        "sil8L",/
     *        "q8L",/
     *        "h2oL",/
     *        "nasGL",/     
     *        "abL",/
     *        "diL",/  
     *        ,/ 
     *        "y",/  
     *        "solution_model.dat",/ 
     *        "Opx(HP)",/ 
     *        "Cpx(HP)",/ 
     *        "Sp(HP)",/      
     *        "O(HP)",/
     *        "Pl(h)",/
     *        "Wad(HP)",/
     *        "Gt(stx)",/
     *        "Atg(PN)",/
     *        "T",/
     *        ,/ 
     *        "calculationtitle",/)
	close (1)
	
	             endif


c**************************************************************
                                    
******************************************************************
c    ...running build.exe...
	call system("build < Buildfile.txt")
	
******************************************************************
     
c   ...writing the file for vertex.exe ...
      open(2,file="vertex_in.txt")
      write(2,1000)datfil
	write (*,*)''
	close(2)

c    ...running vertex.exe...	 
	call system("vertex < vertex_in.txt")
       
*****************************************************************
c    ...writing the file for werami.exe ...	
c    ....CAUTION!!!!!!!!!!!!!! make sure the P_T file
c     has the right P-T range...check the names of minerals!!!!

       if(kkolor.eq.1)then
     	 open(3,file="run_werami1.txt")
       write(3,1000)datfil
1000  format(A20)
       write(3,9002)
9002   format( "2",/ 
     *        "36",/  
     *        "1",/ 
     *        "n",/  
     *        "n",/       
     *        "4",/
     *        "y",/
     *        "0" )
	 close(3)
      
	                              elseif(kkolor.eq.0)then
	     	 open(3,file="run_werami1.txt")
       write(3,1000)datfil
       write(3,2002)
2002   format( "2",/ 
     *        "36",/  
     *        "1",/ 
     *        "n",/
     *        "4",/
     *        "y",/
     *        "0" )
	 close(3)
	                                   endif

c    ...running werami.exe...
	call system("werami < run_werami1.txt")
      
	call system('mv  Buildfile_1.tab TABLE ')
 
c  ... check for FULL table of props...
      if(char2.eq.1)then
                                   if(kkolor.eq.1)then
     	 open(4,file="run_werami2.txt")
       write(4,1000)datfil
       write(4,2010)
2010   format( "2",/ 
     *        "36",/  
     *        "3",/ 
     *        "y",/  
     *        "n",/  
     *        "4",/
     *        "y",/
     *        "0" )
	 close(4)

                                  elseif(kkolor.eq.0)then
     	 open(4,file="run_werami2.txt")
       write(4,1000)datfil
       write(4,2017)
2017   format( "2",/ 
     *        "36",/  
     *        "3",/ 
     *        "n",/  
     *        "n",/  
     *        "4",/
     *        "y",/
     *        "0" )
	 close(4)
                                       endif
c    ...running werami.exe...
	call system("werami < run_werami2.txt")
	call system('mv  Buildfile_1.phm Table_FULL ')
      else
	endif
      


c ... ignore this but don;t delete it
      goto 1245  
c***************************************************************
c     !!!!!!!!!!!!!!!!!!!!!IMPORTANT!!!!!!!!!!!!!!!!!!!!!!
c!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!   
c ...choose the properties you want here by uncommenting/commenting
c     these lines...be careful at how to do it!!
c***************************************************************


c***************************************************************
c  ...preparing the folders and files...

 
1245  ncases=200000
	allocate(jca(ncases),R0(ncases),
     *    R1(ncases),R2(ncases),R3(ncases),
     *    R4(ncases),R5(ncases),
     *    R6(ncases),R7(ncases),R8(ncases),
     *    R9(ncases),R10(ncases),
     *    R11(ncases),R12(ncases),R13(ncases),
     *    R14(ncases),R15(ncases),
     *    R16(ncases),R17(ncases),R18(ncases),
     *    R19(ncases),R20(ncases),
     *    R21(ncases),R22(ncases),R23(ncases),
     *    R24(ncases),R25(ncases),
     *    R26(ncases),R27(ncases),R28(ncases),
     *    R29(ncases))

      open(10,file='TABLE_LITMOD')
	
      OPEN(20,file='TABLE')
	 read(20,*)
	 do hh= 1,14
	  read(20,*)	 
          enddo 
          do ii= 1,200000
          read(20,*,iostat=lkj)R1(ii),
     *     R2(ii),R3(ii),R4(ii),
     *     R5(ii),R6(ii),R7(ii),R8(ii),
     *     R9(ii),R10(ii),R11(ii),R12(ii),
     *     R13(ii),R14(ii),R15(ii),
     *     R16(ii),R17(ii),R18(ii),R19(ii),
     *     R20(ii),R21(ii),R22(ii),
     *     R23(ii),R24(ii),R25(ii),R26(ii),
     *     R27(ii),R28(ii),R29(ii)

           if(lkj.lt.0.0E0)goto 549

      if(R12(ii).lt.100.0)then
	 if(R12(ii-1).lt.100.0)then
	 print *, 'Two consecutive erros in table... ABORT!!!!'
	 print *, 'Try modifying slightly your bulk composition'
	 stop
	 endif

	print *, 'Corrected table at T-P', R1(ii-1)+rrr,R2(ii-1) 
!	write(999, *) 'Corrected table at T-P', R1(ii-1)+rrr,R2(ii-1) 
      R12(ii)=R12(ii-1) + (R12(ii-1) - R12(ii-2))
	R1(ii)=R1(ii-1) + ( R1(ii-1)-R1(ii-2) )
	R2(ii)=R2(ii-1)
	R9(ii)=R9(ii-1)  
	R10(ii)=R10(ii-1) 
	R28(ii)=R28(ii-1)
	R29(ii)=R29(ii-1)
	R25(ii)=R25(ii-1)
	R26(ii)=R26(ii-1)
	rrr=R1(ii-1)-R1(ii-2)
	endif

	write(10,1010)R1(ii),R2(ii),R12(ii),R9(ii),R10(ii),R25(ii),
     *                     R26(ii),R28(ii),R29(ii)
                          
1010  format(F9.3,1F17.4,1F15.3,2F12.3,4E17.6)
      
	enddo
      
549    CLOSE(20)
       close(10)
       deallocate(jca,
     *    R1,R2,R3,R4,R5,
     *    R6,R7,R8,R9,R10,
     *    R11,R12,R13,R14,R15,
     *    R16,R17,R18,R19,R20,
     *    R21,R22,R23,R24,R25,
     *    R26,R27,R28,R29)


c ... thermal conductivity 
      call thermal(kkolor,datfil,rrr)


c ... call anelstic attenuation
      write(*,*)"Now including anelastic attenuation effects."
      write(*,*)"Enter the grain size (5,10) in mm :"
      read(*,*) DSIZE
      write(*,*)"Oscillation period for anelastic effects(50,75,100) s:"
      read(*,*) IOSPE
      call generator_table_atten_corr(DSIZE,IOSPE)
      
1911  format(F9.3,1F17.4,1F15.3,2F12.3,4E17.6,1F8.2)
      open(97,file='TABLE_LITMOD2_atten_corr')
	open(98,file='testtable')
	open(99,file='TABLE_LITMOD2')

      do i=1,2000000
	  read(97,*,iostat=lkj)aa1,aa2,aa3,aa4,aa5,aa6,aa7,aa8,aa9
         if(lkj .lt.0.0E0) goto 1189     

	  do while (iko .ne. 1)
	  read(98,*,iostat=ljk)bb1,bb2,bb3
	  if(ljk .lt.0.0E0) goto 1188
         if(int(aa1) .eq. int(bb1) .and. bb2.eq.aa2)then
	    iko=1
         else
          iko=0
         endif 
	  enddo

	write(99,1911)aa1,aa2,aa3,aa4,aa5,aa6,aa7,aa8,aa9,bb3 
      iko=0    
	bbb=bb3
	goto 8976
1188  write(99,1911)aa1,aa2,aa3,aa4,aa5,aa6,aa7,aa8,aa9,bbb 
      rewind(98)    

8976  iko=0
	enddo

1189  close(97);close(98);close(99) 

c adding some info at the top of table
1013  format(F9.3,1F17.4,1F15.3,2F12.3,4E17.6,1F8.2)
      open(1,file='TABLE_LITMOD2')
      open(2,file='TABLE_LITMOD_use')
      write(2,*)"Comments untill line number 12"
      write(2,*)"Petrological Info:"
      write(2,*)"Database: H&P98 Generator_LitMod2D_2.0_X"
      if(kkolor.eq.1) then
          write(2,*)"SiO2,Al2O3,FeO,MgO,CaO,Na2O,K2O,H2O"
          write(2,*)aSi, bAl, cFe, dMg, eCa, fNa, gK, h20
      else
        write(2,*)"SiO2,Al2O3,FeO,MgO,CaO,Na2O"
        write(2,*)aSi, bAl, cFe, dMg, eCa, fNa
      endif
      write(2,*)"Pressure range(bar):"
      write(2,*) premin,premax
      write(2,*)"Temperature range(K):"
      write(2,*) temmin,temmax
      write(2,*)"Attenuation : "
      write(2,*)"Grain size(mm) =" ,DSIZE
      write(2,*)"Time period (secods) =", IOSPE
      
      do 20
      read(1,1013,iostat=ios)aT,aP,aden,vp,vs,avpdt,avsdt,avpdp,avsdp,bb
      if(IOS.lt.0.0)then
        goto 30
      else 
       write(2,1013)aT,aP,aden,vp,vs,avpdt,avsdt,avpdp,avsdp,bb
c 1003 format(F9.3,1F17.4,2F4.4,2F4.4,2F4.4,2F4.4,2F4.4,2F4.4,2F4.4)
c1010  format(F9.3,1F17.4,1F15.3,2F12.3,4E17.6)
      endif
  20    continue
 30    close(1)
      close(2)
c... clean stuff

      datfil1=TRIM(datfil)//'_'//char
            
      call system('mkdir '//TRIM(datfil1))

      call system('mv  Buildfile.txt ' 
     *          //TRIM(datfil1))

	call system('mv  Buildfile.dat '
     *   //TRIM(datfil1))

      call system('mv  run_werami1.txt '
     *              //TRIM(datfil1))

      call system('mv  run_werami2.txt '
     *              //TRIM(datfil1))

	call system('mv  run_werami3.txt '
     *              //TRIM(datfil1))
  
	call system('rm TABLE ')

c	call system('rm Table_thermo ')
	
      call system('mv  testtable TABLE_k ')

	call system('mv  TABLE_k '
     *         //TRIM(datfil1))
  
	call system('mv  TABLE_LITMOD '
     *         //TRIM(datfil1))
	call system('mv  TABLE_LITMOD2 '
     *         //TRIM(datfil1))
	call system('mv  TABLE_LITMOD_use '
     *         //TRIM(datfil1))
	call system('mv  TABLE_LITMOD2_atten_corr '
     *         //TRIM(datfil1))
	call system('mv  Table_FULL '
     *         //TRIM(datfil1))

c      call system('mv  Table_thermo'
c     *              //TRIM(datfil1))

c      call system('mv  Buildfile_1.phm '
c     *              //TRIM(datfil1))

      call system('mv  vertex_in.txt '
     *              //TRIM(datfil1))

c      call system('mv  auto_refine_Buildfile.dat.txt '
c     *              //TRIM(datfil1))

c      call system('mv  auto_refine_Buildfile.dat '
c     *              //TRIM(datfil1))

c      call system('mv  auto_refine_Buildfile.dat_true_or_false '
c     *              //TRIM(datfil1))

      call system('mv  pseudocompound_glossary.dat '
     *              //TRIM(datfil1))

c	call system('del auto_refine_Buildfile.dat.txt ')
c	call system('del auto_refine_Buildfile.dat ')
c	call system('del auto_refine_Buildfile.dat_true_or_false ')
c	call system('del pseudocompound_glossary.dat ')

c	call system('del run_werami1.txt')

        WRITE(*,*) 'DONE!!!!!!!!!'
	 WRITE(*,*) '    check your beautiful tables...      '
2395       stop
	end







c----------------------------------------------------------------------
      SUBROUTINE thermal(kkolor,datfil,dt)
	
c  ... calculates the thermal conductivity of the assemblage
      
      implicit double precision (a-h,o-z)
	integer kkolor,hh,oo
	CHARACTER(2) jca
	character(100)datfil
	real*8 ck(2),pres,tem
         real*8 R1(2),R2(2)
         

c ... description of the variables:
c R0 counter for full table only
c R1 = T (K)
c R2 = P (bar)
c R3 = molar vol (J/bar)
c R4 = Enthalpy (J)
c R5 = Gruneisen parameter
c R6 = adiabatic bulk modulus (bar)
c R7 = adiabatic shear modulus (bar)
c R8 = bulk velocity (km/s)
c R9 = compressional velocity (km/s)
c R10 = shear velocity (km/s)
c R11 = vp/vs ratio
c R12 = density (kg/m3)
c R13 = unused
c R14 = Cp (J/K)
c R15 = CTE (1/K)
c R16 = compressibility (1/bar)
c R17 = molar Entropy (J/K)
c R18 = number of moles
c R19 = ...
c R20 = T-derivative of Ks (bar/K)
c R21 = T-derivative of Gs (bar/K)
c R22 = P-derivative of Ks 
c R23 = P-derivative of Gs
c R24 = T-derivative of V0
c R25 = T-derivative of Vp
c R26 = T-derivative of Vs
c R27 = P-derivative of V0
c R28 = P-derivative of Vp
c R29 = P-derivative of Vs
c R30 = wt%
c R31 = vol%
c R32 = mol%

1000  format(A20)

        ck(1)=3.3d0 ! previous value
	ck(2)=0.d0   ! actual value
	tem=0.d0
	pres=0.d0
                  
                                   if(kkolor.eq.1)then
     	 open(4,file="run_werami3.txt")
       write(4,1000)datfil
       write(4,2010)
2010   format( "2",/ 
     *        "36",/  
     *        "3",/ 
     *        "n",/  
     *        "n",/  
     *        "1",/
     *        "y",/
     *        "0" )
	 close(4)

                                  elseif(kkolor.eq.0)then
     	 open(4,file="run_werami3.txt")
       write(4,1000)datfil
       write(4,2017)
2017   format( "2",/ 
     *        "36",/  
     *        "3",/ 
     *        "n",/  
     *        "n",/  
     *        "4",/
     *        "y",/
     *        "0" )
	 close(4)
                                       endif
c    ...running werami.exe...
	call system("werami < run_werami3.txt")
	call system('mv  Buildfile_1.phm Table_thermo ')
	call system("sed -i 's/NaN/0/g' Table_thermo ") ! ajout par moi mais faut bugger
      
      OPEN(33,file='testtable')
      OPEN(20,file='Table_thermo')
      open(100,file='ol.dat')
      open(200,file='opx.dat')
      open(300,file='cpx.dat')
      open(400,file='gt.dat')
      open(500,file='sp.dat')
      cond_ol=0d0
	cond_opx=0d0
	cond_cpx=0d0
	cond_gt=0d0
	cond_sp=0d0
	cond_an=0d0
	cond_chl=0d0
	cond_glt=0d0
	cond_c2=0d0
	cond_kn=0d0
	cond_A_p=0d0
	cond_t=0d0
	cond_atg=0d0
	cond_wad=0d0
	cond_san=0d0
	olivf=0d0
	cpxvf=0d0
	opxvf=0d0
	plvf=0d0
	gtvf=0d0
	chlvf=0d0
	gltrf=0d0
	spvf=0d0
	watvf=0d0
	aknvf=0d0
	tvf=0d0
	avf=0d0
	atgvf=0d0
	wadvf=0d0
	sanvf=0d0
	oo=14

	 do hh= 1,14
	  read(20,*)	 
         enddo 

c      do ii= 1,3000000
        read(20,*,iostat=lkj)jca,R0,R1(1),R2(1),R3,R4,R5,
     *     R6,R7,R8,R9,R10,R11,R12,R13,
     *     R14,R15,R16,R17,R18,R19,R20,
     *     R21,R22,R23,R24,R25,R26,R27,
     *     R28,R29,R30,R31,R32

           do while(lkj.eq.0.0E0)
     		oo=oo+1
c		print *, 'ligne du fichier', oo
c		print *, 'lkj', lkj

  		    if(R14.ne.0.d0)then  ! minimization correct 

   			   if(jca.ne.'sy')then !----------------------------------------
 			    	if(jca.eq.'O(')then
				call OLIVINE(R1(1)-273, R2(1)*1.d-4, R14, R12, cond_ol) 
				olivf=R31 /100.d0  ! vol frac
c				write(*,*) 'olivf et cond_ol',olivf,cond_ol
c				write(*,*) 'print',jca,R5,R6,R21,R31
				
				elseif(jca.eq.'Cp')then
      				call DIOPSIDE(R1(1)-273.15, R2(1)*1.d-4, R14, R12, cond_cpx)
				cpxvf=R31 /100.d0
c				write(*,*) 'cpxvf et cond_cpx',cpxvf,cond_cpx			
	
				elseif(jca.eq.'Sp')then
        			call SPINEL(R1(1)-273.15, R2(1)*1.d-4, R14, R12,cond_sp)
c				cond_sp=2.0d0
				spvf=R31 /100.d0
c				write(*,*) 'spvf et cond_sp',spvf,cond_sp
   	
c				elseif(jca.eq.'di')then
c			        call DIOPSIDE(R1(1)-273, R2(1)*1.d-4, R14, R12, cond_cpx)
c				cond_cpx=3.0d0
c				cpxvf=R31 /100.d0

				elseif(jca.eq.'Op')then
				call ORTENSTATITE(R1(1)-273, R2(1)*1.d-4, R14, R12,cond_opx)
				cond_opx=3.0d0
				opxvf=R31 /100.d0
c				write(*,*) 'opxvf et cond_opx',opxvf,cond_opx

				elseif(jca.eq.'Pl')then
				call ANORTITE(R1(1)-273, R2(1)*1.d-4, R14, R12, cond_an)
				plvf=R31 /100.d0

				elseif(jca.eq.'Gt')then
				call GARNET(R1(1)-273, R2(1)*1.d-4, R14, R12, cond_gt)
			  	gtvf=R31 /100.d0 
c			 	write(*,*) 'gtvf et cond_gt',gtvf,cond_gt
			
				elseif(jca.eq.'Ch')then
				cond_chl=2.0d0
				chlvf=R31 /100.d0
		     
			        elseif(jca.eq.'gl')then
				cond_glt=2.0d0
				gltrf=R31 /100.d0

				elseif(jca.eq.'cl')then
				cond_clin=2.0d0
				clinvf=R31 /100.d0            

				elseif(jca.eq.'T ')then
				cond_t=3.0d0 !default ?? à modif
				tvf=R31 /100.d0

				elseif(jca.eq.'pa')then
			         cond_A_p=3.0d0 !default ?? à modif
				avf=R31 /100.d0
				
			        elseif(jca.eq.'At')then
				cond_atg=2.0d0  !default ?? à modif ATg(PN)
				atgvf=R31 /100.d0
c			 	write(*,*) 'R31 Atgvf et cond_atg',R31,atgvf,cond_atg
			
				elseif(jca.eq.'tr')then
				cond_wad=4.0d0
			 	wadvf=R31 /100.d0

				elseif(jca.eq.'H2')then
				cond_wat=1.0d0 !default pour de l'eau ??
			 	watvf=R31 /100.d0
c				write(*,*) 'R31 , watvf et cond_wat',R31,watvf,cond_wat
c				write(*,*) 'print',jca,R5,R6,R21,R31
				
				
c				elseif(jca.eq.'Sa')then
c			       call ALBITE(R1(1)-273.15, R2(1)*1.d-4, R14, R12, cond_san)
c			       cond_san=2.0d0
c				sanvf=R31 /100.d0

				endif !end du if ligne 838 (si jca=O() ==> si minéral reconnu
     				R1(2)=R1(1)
				R2(2)=R2(1)
      

   			 else    ! finished current assemblage (si jca=sy --> system : nouvelles conditions P et T)

			      !calc the aggregate prop. with mixture model
			      ck(2)=cond_ol*olivf+cond_opx*opxvf+cond_cpx*cpxvf
*  				+cond_gt*gtvf+cond_an*plvf+cond_sp*spvf+cond_san*sanvf
*				+cond_chl*chlvf+cond_glt*gltrf+cond_clin*clinvf
*				+cond_wad*wadvf+cond_wat*watvf+cond_t*tvf
*				+cond_A_p*avf+cond_atg*atgvf
     
     
			      contr=olivf+opxvf+spvf+watvf
			      contr=contr+clinvf+gtvf+cpxvf
			      contr=contr+evf+atgvf+avf+sanvf
			      contr=contr+gltvf+chlvf+wadvf+tvf+plvf
!!Ici soucis avec l'addition, n'additionne pas tous les termes
			      if(contr .lt. 0.97 .or. contr .gt. 1.01d0)then
			      print *, 'ligne du fichier', oo
			      print *, 'missing phases?... total vol is', contr
			      print *, 'vol fracts ol opx cpx gt sp plag'
			      print *,olivf,opxvf,cpxvf,gtvf,spvf,plvf
			      print *, 'vol fracts san wad wat T Atg parg' 
			      print *,sanvf,wadvf,watvf,tvf,atgvf,avf
			      endif
!     *     +cond_chl*chlvf+cond_glt*gltrf+cond_kn*aknvf+cond_A_p*avf
!     *     +cond_t*tvf+cond_atg*atgvf

			      write(33,*)R1(2),R2(2),ck(2)
			      write(100,*)R1(2),R2(2),cond_ol
			      write(200,*)R1(2),R2(2),cond_opx
			      write(300,*)R1(2),R2(2),cond_cpx
			      write(400,*)R1(2),R2(2),cond_gt
			      write(500,*)R1(2),R2(2),cond_sp

			!...reset variables for new assemblage
				cond_ol=0d0
				cond_opx=0d0
				cond_cpx=0d0
				cond_gt=0d0
				cond_sp=0d0
				cond_an=0d0
				cond_chl=0d0
				cond_glt=0d0
				cond_c2=0d0
				cond_kn=0d0
				cond_A_p=0d0
				cond_t=0d0
				cond_atg=0d0
				cond_wad=0d0
				cond_san=0d0
				cond_clin=0d0
				olivf=0d0
				cpxvf=0d0
				opxvf=0d0
				plvf=0d0
				gtvf=0d0
				chlvf=0d0
				gltrf=0d0
				spvf=0d0
				watvf=0d0
				aknvf=0d0
				tvf=0d0
				avf=0d0
				atgvf=0d0
				wadvf=0d0
				sanvf=0d0
				clinvf=0d0
                          
			 endif !--------------------------------------- du if ligne 837 (ne system)
      
 		   R1(2)=R1(1)
		   R2(2)=R2(1)
		   ck(1)=ck(2)  ! save current value for later use 
		   pres=R2(1)
		   tem=R1(1)

		else  ! minimization failed du if ligne 835
!****************************************************
	 print *, 'Two consecutive erros in table... MINIMIZATION!!!!'
		      ck(2)=cond_ol*olivf+cond_opx*opxvf+cond_cpx*cpxvf
*		     +cond_gt*gtvf+cond_an*plvf+cond_sp*spvf+cond_san*sanvf
*		     +cond_c2*c2vf+cond_wad*wadvf

		       !cond_chl*chlvf
		  !   *     +cond_glt*gltrf+cond_c2*c2vf+cond_kn*aknvf+cond_A_p*avf
		  !   *     +cond_t*tvf+cond_atg*atgvf+cond_wad*wadvf+cond_san*sanvf 

		      write(33,*)R1(2),R2(2),ck(2)
		      write(100,*)R1(2),R2(2),cond_ol
		      write(200,*)R1(2),R2(2),cond_opx
		      write(300,*)R1(2),R2(2),cond_cpx
		      write(400,*)R1(2),R2(2),cond_gt
		      write(500,*)R1(2),R2(2),cond_sp

		      write(33,*)R1(2)+dt,R2(2),ck(2)
		      write(100,*)R1(2)+dt,R2(1),ck(1)
		      write(200,*)R1(2)+dt,R2(1),ck(1)
		      write(300,*)R1(2)+dt,R2(1),ck(1)
		      write(400,*)R1(2)+dt,R2(1),ck(1)
		      write(500,*)R1(2)+dt,R2(1),ck(1)
        
		      read(20,*)
c			R1(2)=R1(1)
c		       R2(2)=R2(1)
		cond_ol=0d0
		cond_opx=0d0
		cond_cpx=0d0
		cond_gt=0d0
		cond_sp=0d0
		cond_an=0d0
		cond_chl=0d0
		cond_glt=0d0
		cond_c2=0d0
		cond_kn=0d0
		cond_A_p=0d0
		cond_t=0d0
		cond_atg=0d0
		cond_wad=0d0
		cond_wat=0d0
		cond_san=0d0
		olivf=0d0
		cpxvf=0d0
		opxvf=0d0
		plvf=0d0
		gtvf=0d0
		chlvf=0d0
		gltrf=0d0
		spvf=0d0
		c2vf=0d0
		aknvf=0d0
		tvf=0d0
		avf=0d0
		atgvf=0d0
		wadvf=0d0
		watvf=0d0
		sanvf=0d0
!*****************************************************

		ck(1)=ck(2)  ! save current value for later use 
		pres=R2(1)
		tem=R1(1)
	
		endif !ligne 835 (Minimization ++)
		
		
        read(20,*,iostat=lkj)jca,R0,R1(1),R2(1),R3,R4,R5,R6,R7,R8,R9,
     *     R10,R11,R12,R13,R14,R15,R16,R17,R18,R19,R20,
     *     R21,R22,R23,R24,R25,R26,R27,R28,R29,R30,
     *     R31,R32
		
		
		enddo
c	enddo ! ligne 826
      
	 CLOSE(20)
	 CLOSE(33)
 	close(100)
 	close(200)
 	close(300)
 	close(400)
 	close(500)

      return
	end
