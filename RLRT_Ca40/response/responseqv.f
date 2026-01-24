      PROGRAM RESPONSEQV

      IMPLICIT NONE

      real*8 Z, A, Q2, W2, xb, qv, nu, dnu, F1, FL, RT, RL, RTE, RLE
      real*8 nuel, ex, RTQE, RLQE, RTIE, RLIE, RTNS, RLNS, RTTOT, RLTOT
      real*8 flNS, f1NS, fLt, f1t, mp/0.938273/
      real*8 K/0.6/, x, modifier
      integer i,j,type
      integer io_status, arg_status, unit
      character(len=30) filename

      real*8 xvalc(45) /     
     & 0.91648E-01,0.12714E+02,0.13380E+00,0.69068E+01,0.77023E+00,
     & 0.76437E-01,0.87115E+01,0.18976E+01,0.66472E+00,-.39215E+01,
     & 0.99320E+00,0.98312E+00,0.10302E+01,0.10009E+01,0.10000E+01,
     & 0.10070E+01,0.97472E+00,0.10059E+01,0.98892E+00,0.99434E+00,
     & 0.10000E+01,0.99596E+00,0.10028E+01,0.10122E+01,0.10045E+01,
     & 0.79845E+00,0.11295E-05,-.97071E+00,0.92502E+00,0.20146E+01,
     & 0.24416E+01,0.24499E+01,0.31154E+01,0.72998E+00,0.26000E+00,
     & 0.76502E-02,0.29000E+00,0.31429E-01,0.58780E-01,-.15059E+00,
     & 0.38790E-01,0.77051E-01,0.26795E+00,0.17673E+00,0.10451E-01 /
      
      A = 12.0
      Z = 6.0

      call get_command_argument(1, filename, arg_status)
      unit = 20
      open(UNIT=unit, FILE=filename, STATUS='old', IOSTAT=io_status)
      if (io_status/= 0) then
        print *, 'Unable to open file:',filename
        stop
      endif

      i = 0

      do

        read(unit,*,IOSTAT=io_status) i, qv, nu
        if (io_status /= 0) exit

        q2 = qv*qv - nu*nu
        if(q2.LE.0.0) then
            return
        endif
        w2 = mp*mp+2.0*mp*nu-q2
        xb = q2/2.0/mp/nu
        nuel = q2/2./(0.931494*40.0)
        ex = nu-nuel
        
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
        RTTOT = RTTOT - RTIE
        RLTOT = RLTOT - RLIE
        x = q2/(2.0*mp*nu)
        modifier = 1.0 + K*(4.0/26.0)*(1.0 - 0.75*x)
        RTIE = RTIE * modifier
        RLIE = RLIE * modifier
        RTTOT = RTTOT + RTIE
        RLTOT = RLTOT + RLIE
        
        type = 4
        call csfitcomp(w2,q2,A,Z,XVALC,type,f1,fL) !!!  TE response
        fL = 2.0*xb*fL  
        RTE = 2.0/mp*F1/1000.0
        RLE = 0.0
        
        fLNS = 0.0
        f1NS = 0.0
        do j=2,21
            call nuc12sf(Z,A,nu,q2,j,f1t,fLt)

          fLNS = fLNS + fLt
          f1NS = f1NS + f1t      
        enddo
        RTNS = 2.0/mp*F1NS/1000.0 
        RLNS =  qv*qv/q2/2.0/mp/xb*FLNS/1000.0

      !  if(ex.LE.0.012) then  !!! Only needed for plotting purposes
      !     RTNS = RTNS/6.0
      !     RLNS = RLNS/6.0
      !  endif
        
        RLTOT = RLTOT+RLNS
        RTTOT = RTTOT+RTNS
        
        if(q2.GT.0.0) 
     &       write(6,2000) qv,q2,ex,nu,RTTOT,RLTOT,RTQE,RLQE,RTIE,RLIE,
     &                      RTE,RLE,RTNS,RLNS  

      enddo         

 2000  format(4f9.5,10E15.7)
      close(UNIT=unit)
      return
      end
