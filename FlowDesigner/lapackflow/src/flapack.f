
      SUBROUTINE SOLVE(N, NRHS, A, B, INFO)
      INTEGER N
      INTEGER NRHS
      REAL A(N,N)
      REAL B(N,NRHS)
      INTEGER INFO

      INTEGER I
      INTEGER LDA
      INTEGER IPIV(N)
      INTEGER LDB
      LDA=N
      LDB=N
      DO I=1,N
         IPIV(I)=I
      ENDDO
      call SGETRF(N, N, A, N, IPIV, INFO)
      call SGETRS('N', N, NRHS, A, LDA, IPIV, B, LDB, INFO)
      RETURN
      END

      SUBROUTINE EIG(N, A, D, V)
      REAL D(N)
      REAL E(N)
      REAL TAU(N-1)
      REAL WORK(18*N)
      INTEGER INFO
      REAL FDUMMY
      INTEGER IDUMMY
      INTEGER IDUMMY2
      REAL ISUPPZ(2*N)
      INTEGER IWORK(10*N)
      call SSYTRD('U', N, A, N, D, E, TAU, WORK, 18*N, INFO)
      call SORGTR('U', N, A, N, TAU, WORK, 18*N, INFO)
c      print A(1,1), A(1,2), A(1,3)
      call SSTEGR('V', 'A', N, D, E, FDUMMY, FDUMMY, IDUMMY, IDUMMY, 
     *0, IDUMMY2, D, V, N, ISUPPZ, WORK, 18*N, IWORK, 10*N, INFO)

c     ssyev

c     *DLAMCH, IDUMMY, D, V, N, ISUPPZ, WORK, 18*N, IWORK, 10*N, INFO)
      RETURN
      END
