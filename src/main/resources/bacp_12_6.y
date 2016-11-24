Problem: bacp_12_6
Data n_courses := 66; n_periods := 6;
    load_per_period_lb := 10; load_per_period_ub := 24;
    courses_per_period_lb := 2; courses_per_period_ub := 10;
Domains
    Dom periods=[1..n_periods];
    Dom addload=[load_per_period_lb..load_per_period_ub];
    Dom addcourses=[courses_per_period_lb..courses_per_period_ub];
    Dom load=[1..5]; Dom load_ext=[0..5];
Variables
    IntVar course_load[n_courses]::load;
    IntVar acourses[n_courses]::periods;
    IntVar mload[n_courses,n_periods]::load_ext;
    IntVar load_per_period[n_periods]::addload;
    IntVar course_per_period[n_periods]::addcourses;
Constraints
// courses load
    course_load[1]=1;
    course_load[2]=3;
    // ...
// course prerequisites
    acourses[7] < acourses[1];
    // ...

    Forall(t in [1..n_periods]){
        Count([acourses[j] | k in [1..n_courses]], t, course_per_period[t]);
        Forall(c in [1..n_courses]) {
            If_Then_Else( acourses[c] = t )
                { mload[c,t] = course_load[c]; }
                { mload[c,t] = 0; } ;
        }
        Sum( [ mload[i,t] | i in [1..n_courses] ], load_per_period[t] );
    }