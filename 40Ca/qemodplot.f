      PROGRAM QEMODPLOT

      IMPLICIT none

 
      real*8 x,q2,w2,A,Z,f1mec,f1,f1qe,r,rqe,f2,f2qe,rq3,mp,mp2,rat
      real*8 sum,sum2,sumrat,w2min,w2max,e,eb,theta,ep,epmax,nu,q2c
      real*8 pi,pi2,alpha,flux,sigL,sigT,sigm,eps,fL,fLqe,kappa,de
      real*8 sigtmec, sigmec,sigqe,sigtqe,siglqe,sigie,ebcc,veff,foc
      real*8 epnuc,nuel,nuccs,nuccstot,ev,epv,q2v,w2v,epsv,fluxv,ex
      real*8 sigmnonuc,sin2,cos2,tan2,radcon/0.0174533/
      real*8 sigtot,signonuc
      
      integer i,j,k,l,state,type
      logical thend/.false./
      LOGICAL GOODFIT/.true./,coulomb/.true./
      real*8 nuval(10000)
      real*8 xvalc(100) /          
     & 0.20389E+00,0.25194E+02,0.27480E+00,0.79389E+01,0.83033E+00,
     & 0.17548E+00,0.87115E+01,0.15933E+01,0.67213E+00,-.31672E+01,
     & 0.39743E+00,0.15152E+01,0.53648E+00,0.13156E+01,0.23834E-02,
     & 0.10000E+01,0.33887E+01,0.10716E+01,0.70904E+01,0.36392E+01,
     & 0.00000E+00,0.00000E+00,0.20800E+00,0.13506E-01,0.26227E+00,
     & 0.37426E-01,0.85736E-01,0.45888E-01,-.14891E-02,-.55569E-01,
     & 0.10723E+00,0.11030E+00,0.21484E+00,0.10000E+01,0.61528E-01,
     & 0.94294E+00,0.10021E+00,0.10001E-04,0.10000E+01,0.00000E+00,
     & 0.99042E+00,0.98723E+00,0.10192E+01,0.99649E+00,0.99232E+00,
     & 0.10058E+01,0.98169E+00,0.10030E+01,0.98512E+00,0.99310E+00,
     & 0.10012E+01,0.99680E+00,0.10042E+01,0.10362E+01,0.10057E+01,
     & 0.99009E+00,0.10025E+01,0.10066E+01,0.10093E+01,0.10000E+01,
     & 0.10013E+01,0.11168E+01,0.10251E+01,0.10801E+01,0.10053E+01,
     & 0.10365E+01,0.10000E+01,0.10000E+01,0.10054E+01,0.99769E+00,
     & 0.10004E+01,0.10225E+01,0.10000E+01,0.10098E+01,0.10000E+01,
     & 0.10000E+01,0.10331E+01,0.99868E+00,0.10109E+01,0.10055E+01,
     & 0.98947E+00,0.10183E+01,0.10038E+01,0.99648E+00,0.99729E+00,
     & 0.99682E+00,0.10000E+01,0.10000E+01,0.10000E+01,0.10000E+01,
     & 0.10000E+01,0.10000E+01,0.10000E+01,0.10000E+01,0.10000E+01,
     & 0.10000E+01,0.10000E+01,0.10000E+01,0.10000E+01,0.10000E+01 /
      
      integer io_status1, io_status2, nu_num

      mp = .9382727
      mp2 = mp*mp   
      pi = 3.14159
      pi2 = pi*pi
      alpha = 1./137.

      A = 40.
      Z = 20.

      open(UNIT=1, FILE='input1.txt', STATUS='old', IOSTAT=io_status1)
      if (io_status1 /= 0) then
        print *, 'Unable to open input1.txt'
        stop
      endif
      read(1, *) e, theta
      close(UNIT=1)
      
      open(UNIT=2, FILE='input2.txt', STATUS='old', IOSTAT=io_status2)
      if (io_status2 /= 0) then
        print *, 'Unable to open input2.txt'
        stop
      endif
      nu_num = 0
      l = 1
      do while (.true.)
        read(2, *, IOSTAT=io_status2) nuval(l)
        if (io_status2 /= 0) exit  ! stop reading if end of file
        l = l + 1
        nu_num = nu_num + 1
      enddo
      close(UNIT=2)
      
      sin2 = dsin(radcon*theta/2.0)
      sin2 = sin2*sin2
      cos2 = 1.0-sin2
      tan2 = sin2/cos2
 
      nuel = e*e*sin2      
      nuel = nuel/(8.0/1.00797*mp+e*2)
      epnuc = e-nuel
      epmax = epnuc-0.000
      
      epnuc = A*0.931494*e/(A*0.931494+2.0*e*sin2)  !!! Equivalent to above  !!!

      de = 0.001

      ex = -145.0*de
      do i=1,nu_num
        nu = nuval(i)
        ep = e - nu
        ex = epnuc - ep + 0.005
        q2 = 4.*e*ep*sin2
        w2 =  mp2+2.*mp*nu-q2
        if(coulomb) then
         call vcoul(A,Z,veff)
         foc = 1.0D0 + veff/e
         ev =  e + veff     
         epv = ep + veff
        endif
         
        q2v = 4.*ev*epv*sin2
        epsv = 1./(1. + 2.*(nu*nu+q2v)/q2v*tan2)
        w2v = mp2+2.*mp*nu-q2v
        x = q2v/(w2v-mp2+q2v)
        kappa = abs(w2v-mp2)/2./mp
        fluxv = alpha*kappa/(2.*pi2*q2v)*epv/ev/(1.-epsv)

        type = 1
        do type=1,4
         call csfitcomp(w2v,q2v,A,Z,xvalc,type,sigt,sigL)      
         sigm = fluxv*(sigt+epsv*sigl)
         sigm =  0.3894e3*8.0d0*pi2*alpha/abs(w2v-mp2)*sigm
         sigm = sigm*foc*foc
         if(type.eq.1) then
            sigtot = sigm
         elseif(type.eq.2) then
           sigqe = sigm
         elseif(type.eq.3) then
           sigie = sigm
         elseif(type.eq.4) then
           sigmec = sigm
         endif
        enddo  
 

        nuccstot = 0.0
        do k=1,21
           call nuccs12cs(Z,A,e,ep,theta,k,nuccs)
c           if(q2.GT.0.3) nuccs = 0.0
           nuccstot = nuccstot+nuccs/1000.0
           nuccstot = nuccstot
        enddo
        nuccstot = nuccstot
        signonuc = sigtot
        sigtot = sigtot + nuccstot
        

c        if(ex.LT.0.012) then
c           sigtot = sigtot/6.0
c        endif

         if(ep.GT.0.02.AND.w2.LT.40.) then
            write(6,2000) e,theta,nu,ex,q2,sigtot,sigqe,sigie,
     &                  sigmec,nuccstot,signonuc
         endif

      enddo


 2000 format(5f10.4,6e12.5)
      end
