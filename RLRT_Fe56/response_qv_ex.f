      PROGRAM response_qv_ex

      IMPLICIT NONE

      real*8 Z, A, Q2, W2, xb, qv, nu, dnu, F1, FL, RT, RL, RTE, RLE
      real*8 nuel, ex, RTQE, RLQE, RTIE, RLIE, RTNS, RLNS, RTTOT, RLTOT
      real*8 flNS, f1NS, fLt, f1t, mp/0.938273/
      real*8 massC12/11.1748/
      integer io_status, arg_status, unit
      character(len=30) filename

      integer i,j,type
      real*8 xvalc(45) /     
c     & 0.94187E-01,0.10367E+02,0.13523E+00,0.68783E+01,0.76964E+00,
c     & 0.74171E+00,0.20269E+01,0.20269E+01,0.69603E+00,-.41246E+01,
c     & 0.98677E+00,0.97242E+00,0.10338E+01,0.98935E+00,0.10000E+01,
c     & 0.10041E+01,0.97018E+00,0.10073E+01,0.98889E+00,0.99529E+00,
c     & 0.10000E+01,0.99757E+00,0.10040E+01,0.10191E+01,0.10083E+01,
c     & 0.81949E+00,0.00000E+00,-.95810E+00,0.90514E+00,0.18136E+01,
c     & 0.21799E+01,0.21799E+01,0.27720E+01,0.47927E+00,0.22800E+00,
c     & 0.10133E-01,0.26823E+00,0.38365E-01,0.71421E-01,0.88152E-01,
c     & 0.74219E-01,0.82864E-01,0.29490E+00,0.66002E+00,0.81059E+01 /
c     & 0.94131E-01,0.10746E+02,0.13847E+00,0.69187E+01,0.78080E+00,
c     & 0.11348E+00,0.20269E+01,0.20737E+01,0.70874E+00,-.41560E+01,
c     & 0.99234E+00,0.98155E+00,0.10240E+01,0.99269E+00,0.10000E+01,
c     & 0.10058E+01,0.97857E+00,0.10044E+01,0.99274E+00,0.99636E+00,
c     & 0.10000E+01,0.99844E+00,0.10026E+01,0.10106E+01,0.10050E+01,
c     & 0.78561E+00,0.00000E+00,-.10365E+01,0.92719E+00,0.20435E+01,
c     & 0.19717E+01,0.19779E+01,0.28591E+01,0.50813E+00,0.22800E+00,
c     & 0.11486E-01,0.27182E+00,0.40453E-01,0.75158E-01,0.45838E-01,
c     & 0.68540E-02,0.78288E-01,0.29602E+00,0.57452E+00,-.13525E+00 /
c     & 0.86448E-01,0.12116E+02,0.12905E+00,0.68753E+01,0.76463E+00,
c     & 0.76437E-01,0.87115E+01,0.19440E+01,0.67375E+00,-.40009E+01,
c     & 0.99298E+00,0.98302E+00,0.10302E+01,0.10011E+01,0.10000E+01,
c     & 0.10073E+01,0.97422E+00,0.10058E+01,0.98887E+00,0.99433E+00,
c     & 0.10000E+01,0.99588E+00,0.10026E+01,0.10120E+01,0.10046E+01,
c     & 0.79586E+00,0.11295E-05,-.97614E+00,0.92788E+00,0.20215E+01,
c     & 0.24889E+01,0.24890E+01,0.31432E+01,0.74838E+00,0.22800E+00,
c     & 0.70982E-02,0.25749E+00,0.31475E-01,0.56800E-01,-.13645E+00,
c     & 0.36333E-01,0.75341E-01,0.27892E+00,0.15692E+00,0.10459E+00 /
     & 0.73010E-01,0.10612E+02,0.11337E+00,0.67674E+01,0.74110E+00,
     & 0.76437E-01,0.87115E+01,0.18999E+01,0.75048E+00,-.38472E+01,
     & 0.99281E+00,0.98412E+00,0.10303E+01,0.10018E+01,0.10000E+01,
     & 0.10072E+01,0.97384E+00,0.10063E+01,0.98851E+00,0.99300E+00,
     & 0.10000E+01,0.99661E+00,0.10022E+01,0.10116E+01,0.10039E+01,
     & 0.78136E+00,0.11295E-05,-.10064E+01,0.93493E+00,0.22110E+01,
     & 0.26555E+01,0.26555E+01,0.34117E+01,0.76660E+00,0.24100E+00,
     & 0.65674E-02,0.27018E+00,0.31976E-01,0.56458E-01,-.86596E-01,
     & 0.27536E-01,0.75467E-01,0.31597E+00,0.99112E-01,0.96124E-02 / 
      
      
      A = 12.0
      Z = 6.0
      call get_command_argument(1, filename, arg_status)
      
      unit = 20
      open(UNIT=unit, FILE=filename, STATUS='old', IOSTAT=io_status)
      
      if (io_status/= 0) then
        print *, 'Unable to open file:',filename
        stop
      endif

      
      dnu = 0.0

      ! open(unit=6, file='response_qv_nu_output.txt', status='replace')
      i = 0
      do 
        read(unit,*,IOSTAT=io_status) i, qv, ex
        if (io_status /= 0) exit

        nu = - massC12 + sqrt(massC12*massC12+qv*qv+2*massC12*ex)
        q2 = qv*qv - nu*nu
        nuel = q2/2./(0.931494*A)

        w2 = mp*mp+2.0*mp*nu-q2

        xb = q2/2.0/mp/nu


      !   ex = nu-nuel
        
        type = 1
        call csfitcomp(w2,q2,A,Z,XVALC,type,f1,fL) !!!  total response
        fL = 2.0*xb*fL
        RTTOT = 2.0/mp*F1/1000.0
        RLTOT = qv*qv/q2/2.0/mp/xb*FL/1000.

        type = 2
        call csfitcomp(w2,q2,A,Z,XVALC,type,f1,fL) !!!  QE response
        fL = 2.0*xb*fL
        RTQE = 2.0/mp*F1/1000.0
        RLQE = qv*qv/q2/2.0/mp/xb*FL/1000.0
        
        type = 3
        call csfitcomp(w2,q2,A,Z,XVALC,type,f1,fL) !!!  IE response
        fL = 2.0*xb*fL
        RTIE = 2.0/mp*F1/1000.0
        RLIE =  qv*qv/q2/2.0/mp/xb*FL/1000.0
        
        type = 4
        call csfitcomp(w2,q2,A,Z,XVALC,type,f1,fL) !!!  TE response
        fL = 2.0*xb*fL  
        RTE = 2.0/mp*F1/1000.0
        RLE = 0.0


c        write(6,*) RLTOT,RLIE+RLQE
        
        fLNS = 0.0
        f1NS = 0.0
        do j=2,22
           call nuc12sf(Z,A,nu,q2,j,f1t,fLt)

          fLNS = fLNS + fLt
          f1NS = f1NS + f1t      
        enddo
        RTNS = 2.0/mp*F1NS/1000.0 
        RLNS =  qv*qv/q2/2.0/mp/xb*FLNS/1000.0

c        if(ex.LE.0.012) then  !!! Only needed for plotting purposes
c           RTNS = RTNS/6.0
c           RLNS = RLNS/6.0
c        endif
        
        if(RLNS.LE.1E-40) RLNS = 0.0
        if(RTNS.LE.1E-40) RTNS = 0.0
        
        RLTOT = RLTOT+RLNS
        RTTOT = RTTOT+RTNS
        write(6,2000) i,RTTOT,RLTOT,RTQE,RLQE,RTNS,RLNS      
c        write(6,2000) qv,q2,ex,nu,RTTOT,RLTOT,RTQE,RLQE,RTIE,RLIE,RTE,RLE,RTNS,RLNS          
        
c        if(q2.GT.0.0) 
c     &       write(6,2000) qv,q2,ex,nu,RTTOT,RLTOT,RTQE,RLQE,RTIE,RLIE,
c     &                      RTE,RLE,RTNS,RLNS   

c         if(q2.LE.0.0) 
c     &       write(6,2000) qv,q2,ex,nu,RTTOT,RLTOT,RTQE,RLQE,RTIE,RLIE,
c     &                      RTE,RLE,RTNS,RLNS   

c        i = i+1
      enddo

c 2000  format(4f9.5,10E15.7)
c 2000  format(4f9.5,10E11.3)
c 2001  format(I5,2E11.3)    
c 2002  format(4f9.5,10E11.3)
c 2000  format(1I5,2E15.7)    
 2000 format(I5,6E15.7)  
      close(UNIT=unit)
      return
      end


      
      
      
      
     
CCC-----------------

      
