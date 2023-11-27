let (*prove*) append_cons (h : int) (t : list) (l : list)
= (append (Cons (h, t)) l = Cons (h, append t l))