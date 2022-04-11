# apache airflow installation on an ubuntu machine (aka Docker Container): https://gist.github.com/amoraitis/7ed8fad2c76610a0c0d3eab919b1eea3
from airflow import DAG
from airflow.operators.bash_operator import BashOperator
from datetime import datetime

# create dag

with DAG(dag_id="TASK1_P2822124",
start_date=datetime(2021,1,1),
schedule_interval="@hourly",
catchup=False) as dag:

# Task 1.1: create a variable with the first name and push the result 
    task1 = BashOperator(
        task_id="task1.1",
        bash_command='firstName=anastasios &&'
        'echo "Push firstName via xcom" &&'
        'echo "${firstName}"')

# Task 1.2: create a variable with the last name and push the result
    task2 = BashOperator(
        task_id="task1.2",
        bash_command='lastName=moraitis &&'
        'echo "Push lastName via xcom" &&'
        'echo "${lastName}"')

# Task 2.1: consume the result of 1.1 - 1) print it, 2) store the bash operation's result that capitalizes the first letter, and 3) push it forward via xcomm
    task3 = BashOperator(
        task_id="task2.1",
        bash_command=f'echo "The xcom pushed as firstName is {task1.output}" && '
        f'firstNameCapitalized=$(echo {task1.output} | awk \'{{for(i=1;i<=NF;i++){{ $i=toupper(substr($i,1,1)) substr($i,2) }} }}1\') && '
        'echo "${firstNameCapitalized}"')

# Task 2.2: consume the result of 1.2 - 1) print it, 2) store the bash operation's result that capitalizes the first letter, and 3) push it forward via xcomm
    task4 = BashOperator(
        task_id="task2.2",
        bash_command=f'echo "The xcom pushed as lastName is {task2.output}" && '
        f'lastNameCapitalized=$(echo {task2.output} | awk \'{{for(i=1;i<=NF;i++){{ $i=toupper(substr($i,1,1)) substr($i,2) }} }}1\') && '
        'echo "${lastNameCapitalized}"')

# Task 3: Consume the result of tasks 2.1, 2.2 and print them out with a spece in between.
    task5 = BashOperator(
        task_id="task3",
        do_xcom_push=False,
        bash_command='echo "My Full Name is:"'
        f' {task3.output} {task4.output}')


# parallelize executions, until task 3
task1 >> task3 >> task5

task2 >> task4 >> task5