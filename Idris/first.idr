module first

lemmaAbbA : a = b -> b = a
lemmaAbbA Refl = Refl

lemma0 : (a:Nat) -> (b:Nat) -> S(a + b) = a + S b
lemma0 Z b = Refl 
lemma0 (S k) b = let ih = lemma0 k b in 
                        rewrite ih in Refl

lemma1 : (a:Nat) -> (b:Nat) -> a + S b = S (a + b)
lemma1 a b = lemmaAbbA (lemma0 a b) 

zeroComm : (a:Nat) -> Z + a = a + Z
zeroComm Z = Refl
zeroComm (S k) = cong (zeroComm k)

dualComm : (a:Nat) -> (b:Nat) -> a + b = b + a
dualComm Z b = zeroComm b
dualComm (S k) b = let ih = dualComm k b in 
                      let lem = lemma1 b k in 
                              let prev = cong ih in 
                                      rewrite lem in prev




