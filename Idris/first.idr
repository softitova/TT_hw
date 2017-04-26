module first

lemmaAbbA : a = b -> b = a
lemmaAbbA Refl = Refl

lemma0 : (a:Nat) -> (b:Nat) -> S(a + b) = a + S b
lemma0 Z b = Refl 
lemma0 (S k) b = let ih = lemma0 k b in  -- S (k + b) = k + S b
                        rewrite ih in Refl -- ntp = S (S k + b) = S (k + s b) , we have ih 
                       --  need to find k + sb in ih and rewrite other part of equality into ntp

lemma1 : (a:Nat) -> (b:Nat) -> a + S b = S (a + b)
lemma1 a b = lemmaAbbA (lemma0 a b) 

zeroComm : (a:Nat) -> Z + a = a + Z
zeroComm Z = Refl
zeroComm (S k) = cong (zeroComm k)

dualComm : (a:Nat) -> (b:Nat) -> a + b = b + a
dualComm Z b = zeroComm b
dualComm (S k) b = let ih = dualComm k b in -- k + b = b + k
                      let lem = lemma1 b k in -- b + S k = S (b + k)
                              let prev = cong ih in -- S (k + b) = S (b + k)
                                      rewrite lem in prev -- S (k + b)= b + S k === wwn




