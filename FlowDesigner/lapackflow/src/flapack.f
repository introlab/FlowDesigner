
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
      call SGETRS('N', N, NRHS, A, LDA, IPIV, B, LDB, INFO)
      RETURN
      END



      SUBROUTINE ALLO(A, B)
      REAL A(3,3)
      REAL B(3,1)
      INTEGER I
      
      B(1,1) = A(1,1)
      B(2,1) = A(2,2)
      B(3,1) = A(3,3)
      END


      SUBROUTINE SGETRS2(TRANS, N, NRHS, A, LDA, IPIV, B, LDB, INFO)
c      SUBROUTINE SGETRS2(N, NRHS, A, LDA, IPIV, B, LDB, INFO)
      CHARACTER*(*) TRANS
      INTEGER N
      INTEGER NRHS
      REAL A(N,N)
      INTEGER LDA
      INTEGER IPIV(N)
      REAL B(N,1)
      INTEGER LDB
      INTEGER INFO
      
      
      B(1,1) = A(1,1)
      B(2,1) = A(2,2)
      B(3,1) = A(3,3)
      END

      SUBROUTINE SGETRS3(N, NRHS, A, LDA, IPIV, B, LDB, INFO)
      INTEGER N
      INTEGER NRHS
      REAL A(N,N)
      INTEGER LDA
      INTEGER IPIV(N)
      REAL B(N,1)
      INTEGER LDB
      INTEGER INFO


c      call SGETRS('T', N, NRHS, A, LDA, IPIV, B, LDB, INFO)
      call SGETRS('N', N, NRHS, A, LDA, IPIV, B, LDB, INFO)
      end

      SUBROUTINE SGETRS4()
      CHARACTER*1 TRANS
      INTEGER N
      INTEGER NRHS
      REAL A(3,3)
      INTEGER LDA
      INTEGER IPIV(3)
      REAL B(1,3)
      INTEGER LDB
      INTEGER INFO

      INTEGER TOTO(3)

      REAL AA(3,3)
      REAL BB(3,1)
c      call SGETRS('T', N, NRHS, A, LDA, IPIV, B, LDB, INFO)
      call SGETRS('N', 3, 1, A, 3, TOTO, B, 3, INFO)
      end
