
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
      REAL WORK(28*N)
      INTEGER INFO
      REAL FDUMMY
      INTEGER IDUMMY
      INTEGER IDUMMY2
c      INTEGER IDUMMY3
      REAL ISUPPZ(2*N)
      INTEGER IWORK(10*N)
c      call SSYTRD('U', N, A, N, D, E, TAU, WORK, 18*N, INFO)
c      call SORGTR('U', N, A, N, TAU, WORK, 18*N, INFO)
c      print A(1,1), A(1,2), A(1,3)
c      call SSTEGR('V', 'A', N, D, E, FDUMMY, FDUMMY, IDUMMY, IDUMMY, 
c     *0, IDUMMY2, D, V, N, ISUPPZ, WORK, 18*N, IWORK, 10*N, INFO)

c      CALL SSYEV('V', 'U', N, A, N, D, WORK, 18*N, INFO)
      CALL SSYEVR('V', 'A', 'U', N, A, N, FDUMMY, FDUMMY, IDUMMY, 
     *IDUMMY, DLAMCH, IDUMMY2, D, V, N, ISUPPZ, WORK, 28*N, IWORK, 10*N,
     *INFO)
c      print INFO
c     *DLAMCH, IDUMMY, D, V, N, ISUPPZ, WORK, 18*N, IWORK, 10*N, INFO)
      RETURN
      END
