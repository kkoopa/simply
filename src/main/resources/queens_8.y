Problem: queens_8
    Data
        n:=8;
    Domains
        Dom rows=[1..n];
    Variables
        IntVar q[n]::rows;
    Constraints
        AllDifferent([q[i] | i in [1..n]]);
        Forall(i in [1..n-1]) {
            Forall(j in [i+1..n]) {
                q[i]-q[j]<>j-i;
                q[j]-q[i]<>j-i;
            }
        }