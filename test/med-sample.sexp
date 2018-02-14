((operands ((rA . 'Gprc) (rS . 'Gprc) (rB . 'Gprc))) 
  (in ('XER 'CR rB rS 'IP))
  (defs 
    (('CR 
       (bvor 
         (bvand 
           'CR
           (bvnot (bvshl #x0000000f (bvmul ((_ zero_extend 29) #b000) #x00000004))))
         (bvshl 
           ((_ zero_extend 28) 
             (concat 
               (ite 
                 (bvslt (bvxor rS rB) #x00000000)
                 #b100
                 (ite (bvsgt (bvxor rS rB) #x00000000) #b010 #b001))
               ((_ extract 0 0) 'XER)))
           (bvmul ((_ zero_extend 29) #b000) #x00000004)))) (rA (bvxor rS rB)) ('IP (bvadd 'IP #x00000004)))))