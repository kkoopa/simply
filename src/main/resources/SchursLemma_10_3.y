Problem: SchursLemma_10_3
Data n_balls := 10; n_boxes := 3;
Domains Dom d_boxes = [1..n_boxes];
Variables IntVar putIn[n_balls] :: d_boxes;
Constraints
    Forall(i in [1..n_balls]) {
        Forall(j in [1..n_balls]) {
            Forall(k in [1..n_balls]) {
                If (i+j=k) Then {
                    (putIn[i] <> putIn[j]) Or (putIn[i] <> putIn[k]);
                }
            }
        }
    }
