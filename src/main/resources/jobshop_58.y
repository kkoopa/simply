Problem: jobshop_58
    Data n_machines := 5; n_jobs := 8;
        n_tasks_per_job := 5; max_duration := 58;
    Domains Dom machines=[0..n_machines-1];
            Dom duration=[0..max_duration];
            Dom task_duration=[1..9];
    Variables
        IntVar job_task_start[n_jobs,n_tasks_per_job]::duration;
        IntVar job_task_machine[n_jobs,n_tasks_per_job]::machines;
        IntVar job_task_duration[n_jobs,n_tasks_per_job]::task_duration;
    Constraints
    // machine for jobs and tasks
        job_task_machine[1,1]=1;
        // ...
        Forall (j in [1..n_jobs]) {
            Forall (k in [1..n_tasks_per_job-1]) {
                job_task_start[j, k] + job_task_duration[j, k]
                =< job_task_start[j, k + 1];
            }
        }
        Forall (j in [1..n_jobs]) {
            job_task_start[j, n_tasks_per_job] +
            job_task_duration[j, n_tasks_per_job]
            =< max_duration;
        }
        Forall(ja in [1..n_jobs-1]) {
            Forall(jb in [(ja+1)..n_jobs]) {
                Forall(ka in [1..n_tasks_per_job]) {
                    Forall(kb in [1..n_tasks_per_job]) {
                        (job_task_machine[ja,ka] = job_task_machine[jb, kb])
                            Implies
                        (( job_task_start[ja, ka] + job_task_duration[ja, ka]
                            =< job_task_start[jb, kb] )
                            Or
                            ( job_task_start[jb, kb] + job_task_duration[jb, kb]
                            =< job_task_start[ja, ka] ) );
                    }
                }
            }
        }