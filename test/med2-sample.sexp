((operands
 ((rD . 'GPR)
 (setcc . 'Cc_out)
 (predBits . 'Pred)
 (rM . 'GPR)
 (rN . 'GPR)))
(in (setcc rN rM 'CPSR 'PC))
(defs
 (('PC
  (ite
   ((_ call "arm.is_r15") rD)
   (ite
    (bveq
     #b0
     ((_ extract 0 0)
     ((_ extract 31 0)
     (bvadd
      (bvadd ((_ zero_extend 1) rN) ((_ zero_extend 1) (bvnot rM)))
      ((_ zero_extend 1) #x00000001)))))
    (bvand
     #xfffffffe
     ((_ extract 31 0)
     (bvadd
      (bvadd ((_ zero_extend 1) rN) ((_ zero_extend 1) (bvnot rM)))
      ((_ zero_extend 1) #x00000001))))
    (ite
     (bveq
      #b0
      ((_ extract 1 1)
      ((_ extract 31 0)
      (bvadd
       (bvadd
        ((_ zero_extend 1) rN)
        ((_ zero_extend 1) (bvnot rM)))
       ((_ zero_extend 1) #x00000001)))))
     (bvand
      #xfffffffd
      ((_ extract 31 0)
      (bvadd
       (bvadd
        ((_ zero_extend 1) rN)
        ((_ zero_extend 1) (bvnot rM)))
       ((_ zero_extend 1) #x00000001))))
     ((_ extract 31 0)
     (bvadd
      (bvadd ((_ zero_extend 1) rN) ((_ zero_extend 1) (bvnot rM)))
      ((_ zero_extend 1) #x00000001)))))
   (bvadd 'PC #x00000004)))
 ('CPSR
  (ite
   (ite
    (andp (bveq #b1 ((_ extract 0 0) predBits)) (bvne predBits #xf))
    (notp
     (ite
      (bveq ((_ extract 3 1) predBits) #b000)
      (bveq #b1 ((_ extract 30 30) 'CPSR))
      (ite
       (bveq ((_ extract 3 1) predBits) #b001)
       (bveq #b1 ((_ extract 29 29) 'CPSR))
       (ite
        (bveq ((_ extract 3 1) predBits) #b010)
        (bveq #b1 ((_ extract 31 31) 'CPSR))
        (ite
         (bveq ((_ extract 3 1) predBits) #b011)
         (bveq #b1 ((_ extract 28 28) 'CPSR))
         (ite
          (bveq ((_ extract 3 1) predBits) #b100)
          (andp
           (bveq #b1 ((_ extract 29 29) 'CPSR))
           (notp (bveq #b1 ((_ extract 30 30) 'CPSR))))
          (ite
           (bveq ((_ extract 3 1) predBits) #b101)
           (bveq
            ((_ extract 31 31) 'CPSR)
            ((_ extract 28 28) 'CPSR))
           (ite
            (bveq ((_ extract 3 1) predBits) #b110)
            (andp
             (bveq
              ((_ extract 31 31) 'CPSR)
              ((_ extract 28 28) 'CPSR))
             (notp (bveq #b1 ((_ extract 30 30) 'CPSR))))
            (bveq #b0 #b0)))))))))
    (ite
     (bveq ((_ extract 3 1) predBits) #b000)
     (bveq #b1 ((_ extract 30 30) 'CPSR))
     (ite
      (bveq ((_ extract 3 1) predBits) #b001)
      (bveq #b1 ((_ extract 29 29) 'CPSR))
      (ite
       (bveq ((_ extract 3 1) predBits) #b010)
       (bveq #b1 ((_ extract 31 31) 'CPSR))
       (ite
        (bveq ((_ extract 3 1) predBits) #b011)
        (bveq #b1 ((_ extract 28 28) 'CPSR))
        (ite
         (bveq ((_ extract 3 1) predBits) #b100)
         (andp
          (bveq #b1 ((_ extract 29 29) 'CPSR))
          (notp (bveq #b1 ((_ extract 30 30) 'CPSR))))
         (ite
          (bveq ((_ extract 3 1) predBits) #b101)
          (bveq
           ((_ extract 31 31) 'CPSR)
           ((_ extract 28 28) 'CPSR))
          (ite
           (bveq ((_ extract 3 1) predBits) #b110)
           (andp
            (bveq
             ((_ extract 31 31) 'CPSR)
             ((_ extract 28 28) 'CPSR))
            (notp (bveq #b1 ((_ extract 30 30) 'CPSR))))
           (bveq #b0 #b0)))))))))
   (ite
    (andp (bveq setcc #b1) (notp ((_ call "arm.is_r15") rD)))
    (concat
     (concat
      ((_ extract 31 31)
      ((_ extract 31 0)
      (bvadd
       (bvadd
        ((_ zero_extend 1) rN)
        ((_ zero_extend 1) (bvnot rM)))
       ((_ zero_extend 1) #x00000001))))
      (concat
       (ite
        (bveq
         ((_ extract 31 0)
         (bvadd
          (bvadd
           ((_ zero_extend 1) rN)
           ((_ zero_extend 1) (bvnot rM)))
          ((_ zero_extend 1) #x00000001)))
         #x00000000)
        #b1
        #b0)
       (concat
        ((_ extract 32 32)
        (bvadd
         (bvadd
          ((_ zero_extend 1) rN)
          ((_ zero_extend 1) (bvnot rM)))
         ((_ zero_extend 1) #x00000001)))
        (bvand
         ((_ extract 31 31)
         ((_ extract 31 0)
         (bvadd
          (bvadd
           ((_ zero_extend 1) rN)
           ((_ zero_extend 1) (bvnot rM)))
          ((_ zero_extend 1) #x00000001))))
         ((_ extract 32 32)
         (bvadd
          (bvadd
           ((_ zero_extend 1) rN)
           ((_ zero_extend 1) (bvnot rM)))
          ((_ zero_extend 1) #x00000001)))))))
     ((_ extract 27 0)
     (ite
      ((_ call "arm.is_r15") rD)
      (ite
       (bveq
        #b0
        ((_ extract 0 0)
        ((_ extract 31 0)
        (bvadd
         (bvadd
          ((_ zero_extend 1) rN)
          ((_ zero_extend 1) (bvnot rM)))
         ((_ zero_extend 1) #x00000001)))))
       (bvand #xfeffffff (bvor #x00000020 'CPSR))
       'CPSR)
      'CPSR)))
    (ite
     ((_ call "arm.is_r15") rD)
     (ite
      (bveq
       #b0
       ((_ extract 0 0)
       ((_ extract 31 0)
       (bvadd
        (bvadd
         ((_ zero_extend 1) rN)
         ((_ zero_extend 1) (bvnot rM)))
        ((_ zero_extend 1) #x00000001)))))
      (bvand #xfeffffff (bvor #x00000020 'CPSR))
      'CPSR)
     'CPSR))
   'CPSR))
 (rD
  (ite
   (ite
    (andp (bveq #b1 ((_ extract 0 0) predBits)) (bvne predBits #xf))
    (notp
     (ite
      (bveq ((_ extract 3 1) predBits) #b000)
      (bveq #b1 ((_ extract 30 30) 'CPSR))
      (ite
       (bveq ((_ extract 3 1) predBits) #b001)
       (bveq #b1 ((_ extract 29 29) 'CPSR))
       (ite
        (bveq ((_ extract 3 1) predBits) #b010)
        (bveq #b1 ((_ extract 31 31) 'CPSR))
        (ite
         (bveq ((_ extract 3 1) predBits) #b011)
         (bveq #b1 ((_ extract 28 28) 'CPSR))
         (ite
          (bveq ((_ extract 3 1) predBits) #b100)
          (andp
           (bveq #b1 ((_ extract 29 29) 'CPSR))
           (notp (bveq #b1 ((_ extract 30 30) 'CPSR))))
          (ite
           (bveq ((_ extract 3 1) predBits) #b101)
           (bveq
            ((_ extract 31 31) 'CPSR)
            ((_ extract 28 28) 'CPSR))
           (ite
            (bveq ((_ extract 3 1) predBits) #b110)
            (andp
             (bveq
              ((_ extract 31 31) 'CPSR)
              ((_ extract 28 28) 'CPSR))
             (notp (bveq #b1 ((_ extract 30 30) 'CPSR))))
            (bveq #b0 #b0)))))))))
    (ite
     (bveq ((_ extract 3 1) predBits) #b000)
     (bveq #b1 ((_ extract 30 30) 'CPSR))
     (ite
      (bveq ((_ extract 3 1) predBits) #b001)
      (bveq #b1 ((_ extract 29 29) 'CPSR))
      (ite
       (bveq ((_ extract 3 1) predBits) #b010)
       (bveq #b1 ((_ extract 31 31) 'CPSR))
       (ite
        (bveq ((_ extract 3 1) predBits) #b011)
        (bveq #b1 ((_ extract 28 28) 'CPSR))
        (ite
         (bveq ((_ extract 3 1) predBits) #b100)
         (andp
          (bveq #b1 ((_ extract 29 29) 'CPSR))
          (notp (bveq #b1 ((_ extract 30 30) 'CPSR))))
         (ite
          (bveq ((_ extract 3 1) predBits) #b101)
          (bveq
           ((_ extract 31 31) 'CPSR)
           ((_ extract 28 28) 'CPSR))
          (ite
           (bveq ((_ extract 3 1) predBits) #b110)
           (andp
            (bveq
             ((_ extract 31 31) 'CPSR)
             ((_ extract 28 28) 'CPSR))
            (notp (bveq #b1 ((_ extract 30 30) 'CPSR))))
           (bveq #b0 #b0)))))))))
   (ite
    ((_ call "arm.is_r15") rD)
    rD
    ((_ extract 31 0)
    (bvadd
     (bvadd ((_ zero_extend 1) rN) ((_ zero_extend 1) (bvnot rM)))
     ((_ zero_extend 1) #x00000001))))
   rD)))))
