# Data Definitions, Constraints and Schema Changes
- Used to CREATE, DROP and ALTER the descriptions of the tables (relations) of the database.

## Create table
- Specifies a new base relation by giving it a name, and specifying each of its attributes and their data types (`INTEGER`, `FLOAT`, `DECIMAL(i,j)`, `CHAR(n)`, `VARCHAR(n)`).
- A constant NOT NULL may be specified on an attribute.

> `CHAR(n)` $\rightarrow$ string with max `n` characters
>`VARCHAR(n)` $\rightarrow$ string that contains exactly `n` characters
>`DECIMAL(i,j)` $\rightarrow$ `i` = max figures  -- `j` = max figures after the `,` 

```
CREATE TABLE DEPARTMENT
 (  DNAME VARCHAR(10) NOT NULL,
    DNUMBER INTEGER NOT NULL,
    MGRSSN CHAR(9),
    MGRSARTDATE CHAR(9) );
```

- In SQL2, can use CREATE TABLE command for specifying the primary key attributes, secondary keys and referential integrity constrains (foreign keys).
- Key attributes can be specified via the PRIMARY KEY and UNIQUE phrases.

```
CREATE TABLE DPT
 (   DNAME VARCHAR(10) NOT NULL,
     DNUMBER INTEGER NOT NULL,
     MGRSSN CHAR(9),
     MGRSTARTDATE CHAR(9),
     
	 PRIMARY KEY (DNUMBER), 
	 UNIQUE (DNAME),
	 FOREIGN KEY (MGRSSN) REFERENCES EMP );
```

## CREATE SCHEMA
- Specifies a new database schema by giving it a name.

## DROP TABLE
- Used to remove a relation (bae table) and *its definition*
- The relation can be no longer be used in queries, updates, or any other command since its description no longer exists
- Example:
	- `DROP TABLE DEPENDENT`

## ALTER TABLE
- Used to add an attribute to one of the base relations
- The new attribute will have NULLs in all the tuples of the relation right after the command it executed; hence, the NOT NULL constraint is *not allowed* for such an attribute
- Example:
	- `ALTER TABLE EMPLOYEE ADD JOB VARCHAR(12)`
- The database users must still enter a value for the new attribute JOB for each EMPLOYEE tuple. This can be done using the `UPDATE` command.

# REFERENTIAL INTEGRITY OPTIONS
- We can specify `RESTRICT`, `CASCADE`, `SET NULL` or `SET DEFAULT` on referential integrity constraints (foreign keys).

```
CREATE TABLE DEPT
 (   DNAME VARCHAR(10) NOT NULL, 
     DNUMBER INTEGER NOT NULL, 
     MGRSSN CHAR(9), 
     MGRSTARTDATE CAHR(9), 
     PRIMARY KEY (DNUMBER), 
     UNIQUE (DNAME), 
     FOREIGN KEY (MGRSSN) REFERENCES EMP ON DELETE SET DEFAULT ON UPDATE CASCADE );

CREATE TABLE EMP
(   ENAME VARCHAR(30) NOT NULL,
    ESSN CHAR(9), 
    BDATE DATE, 
    DNO INTEGER DEFAULT 1, 
    SUPERSSN CHAR(9), 
    PRIMARY KEY (ESSN), 
    FOREIGN KEY (DNO) REFERENCES DEPT ON DELETE SET DEFAUT ON UPDATE CASCADE,
    FOREIGN KEY (SUPERSSN) REFERENCES EMP ON DLETE SET NULL ON UPDATE CASCADE );
```

# Additional Data Types in SQL2 and SQL-99
Has `DATE`, `TIME` and `TIMESTAMP` data types.

- `DATE`:
	- Made up of year-month-day in the format `yyyy-mm-dd`
- `TIME`:
	- Made up of `hour:minute:second` in the format `hh:mm:ss`
- `TIME(i)`:
	- Made up of `hour:minute:second` plus `i` additional digits specifying of a second
	- format is `hh:mm:ss:ii...i`
- `TIMESTAMP`:
	- Has both `DATE` and `TIME` components

- `INTERVAL`:
	- Specifies a relative value rather than an absolute value
	- Can be `DAY/TIME` intervals or `YEAR/MONTH` intervals
	- Can be positive or negative when added to or subtracted from an absolute value, the rest is an absolute value

# Specifying Updates in SQL
There are three SQL commands to modify the database:
- `INSERT`
- `DELETE`
- `UPDATE`

## INSERT
- In its simplest form, it is used to add one or more tuples to a relation
- Attribute values should be listed in the same order ad the attributes were specified in the CREATE TABLE command
- *Example*:
		```**U1: INSERT INTO  EMPLOYEE  
		VALUES ('Richard','K','Marini', '653298653', '30-DEC-52',  
		'98 Oak Forest,Katy,TX', 'M', 37000,'987654321', 4 )**```
- An alternate form of `INSERT` specifies explicitly the attribute names that correspond to the values in the new tuples
- Attribute with NULL values can be left out
- **Important Note**: Only the constraints specified in the DDL commands are automatically enforced by the DBMS when updates are applied to the database
- Another variant of INSERT allows insertion of *multiple tuples* resulting from a query into a relation.

## DELETE
- Removes tuples from a relation
- Includes a WHERE-clause to select the tuples to be deleted
- Tuples are deleted from only *one table* at time (unless CASCADE is specified on a referential integrity constraint)
- A missing WHERE-clause specifies that *all tuples* in the relation are to be deleted; the table then becomes an empty table
- The number of tuples deleted depends on the number of tuples in the relation that satisfy the WHERE-clause
- Referential integrity should be enforced
- *Example*:
	```
		**U4A: DELETE FROM EMPLOYEE  
		WHERE LNAME='Brown’  
		  
		U4B: DELETE FROM EMPLOYEE  
		WHERE SSN=‘123456789’  
		  
		U4C: DELETE FROM EMPLOYEE  
		    WHERE  DNO  IN (SELECT DNUMBER  
		FROM DEPARTMENT  
		WHERE DNAME='Research')  
		  
		U4D: DELETE FROM EMPLOYEE**
	```

## UPDATE
- Used to modify attribute values of one or more selected tuples
- A WHERE-clause selects the tuples to be modified
- An additional SET-clause specifies the attributes to me modified and their new values
- Each command modifies tuples *in the same relation*
- Referential integrity should be enforced
  
- *Example*: change the location and controlling department number of project number 10 to 'Bellaire' and 5, respectively
	```
	U5: UPDATE PROJECT
	SET  PLOCATION = 'Bellaire', DNUM = 5
	WHERE PNUMBER = 10
	```

- *Example*: Give all employees in the 'Research' department a 10% raise in salary
	```
	U6: UPDATE EMPLOYEE
	 SET    SALARY = SALARY * 1.1
	 WHERE  DNO IN (SELECT DNUMBER FROM DEPARTMENT WHERE DNAME = 'Research')
	```
- In this request, the modified SALARY value depends on the original SALARY value in each tuple
- The reference to the SALARY attribute on the right of `=` refers to the old SALARY value before modification
- The reference to the SALARY attribute on the left of `=` refers to the new SALARY value after modification

# Retrieval Queries in SQL
- SQL has one basic statement for retrieving information from a database; the SELECT statement
- This is *not the same as* the SELECT operation of the relational algebra
- Important distinction between SQL and the formal relational model; SQL allows a table (relation) to have two or more tuples that are identical in all their attribute values
- Hence, an SQL relation (table) is a *multi-set* (sometimes called a bag) of tuples; it *is not* a set of tuples
- SQL relations can be constrained to be sets by specifying PRIMARY KEY or UNIQUE attributes, or by using the DISTING option in a query.

- Basic form of the SQL SELCT statement is called a *mapping* or a *SELECT-FROM-WHERE block*
	```
	SELECT <attribute list>
	FROM   <table list>
	WHERE  <condition>
	```
	- `<attribute list>` is a list of attributes names whose values are to be retrieved by the query
	- `<table list>` is a list of the relation names required to process the query
	- `<condition>` is a conditional (Boolean) expression that identifies the tuples to be retrieved by the query

# Simple SQL Queries
- Basic SQL queries correspond to using the SELECT, PROJECT, and JOIN operations o the relational algebra
- All subsequent examples use the COMPANY database
- Example of a simple query on *one* relation
- Query 0: Retrive the birthdate and address of the employee whose name is "John B. Smith"
	```
	Q0: SELECT BDATE ADDRESS
	FROM    EMPLOYEE
	WHERE   FNAME='John' AND MINIT='B'
	AND     LNAME='Smith'
	```
	- Similar to SELECT-PROJECT pair of relational algebra operations; the SELECT-clause specifies the *projection attributes* and the WHERE-clause specifies the *selection condition*
	- However, the result of the query *may contain* duplicate tuples

- Query 1: Retrieve the name and address of all employees who work for the 'Research' department
	```
	Q1: SELECT FNAME, LNAME, ADDRESS
	FROM   EMPLOYEE, DEPARTMENT
	WHERE  DNAME='Research' AND NUMBER=DNO
	```
	- Similar to a SELECT-PROJECT-JOIN sequence of relational algebra operations
	- (`DNAME='Research'`) is a *selection condition* (correspond to a SELECT operation in relational algebra)
	- (`DNUMBER=DNO`) is a *join condition* (correspond to a JOIN operation in relational algebra)

- Query 2: For every project located in 'Stafford', list the project number, the controlling department number, and the department manager's last name, address and birthdate.
	```
	Q2: SELECT   PNUMBER, DNUM, LNAME, BDATE, ADDRESS
		FROM     PROJECT, DEPARTMENT, EMPLOYEE
		WHERE    DNUM=DNUMBER AND MGRSSN=SSN    AND    PLOCATION='Stafford'
	```
	- In Q2, there are *two* join conditions
	- The join condition `DNUM=DNUMBER` relates a project to its controlling department
	- The join condition `MGRSSN=SSN` relates the controlling department to the employee who manages that department

# Aliases, * and DISTINCT, Empty WHERE-clause
- In SQL we can use the same name for two (or more) attributes as long as the attributes are in *different relations*. A query that refers to two or more attributes with the same name must *qualify* the attribute name with the relation name by *prefixing* the relation name to the attribute name

Example:
`EMPLOYEE.LNAME, DEPARTMENT.DNAME`

## Aliases
- Some queries need to a refer to the same name relation twice
- In the case, *aliases* are given to the relation name
- Query 8: For each employee, retrieve the employee's name, and the name of his or her immediate supervisor.
	```
	Q8: SELECT   E.FNAME, E.LNAME, S.FNAME, S.LNAME
	    FROM     EMPLOYEE AS E, EMPLOYEE AS S
	    WHERE    E.SUPERSSN=S.SSN
	```
	- In Q8, the alternate relation name E and S are called *aliases* or *tuple variables* for the EMPLOYEE relation
	- We can think of E and S as two *different copies* of EMPLOYEE; E represent employees in role of *supervisees* and S represent employees in role of *supervisor*

- Aliasing can also be used in any SQL query for convenience.
  Can also use the AS keyword to specify aliases.
```
Q8: SELECT   E.FNAME, E.LNAME, S.FNAME, S.LNAME
    FROM     EMPLOYEE AS E, EMPLOYEE AS S
    WHERE    E.SUPERSSN=S.SSN
```

# Unspecified WHERE-clause
- A *missing WHERE-clause* indicates no condition; hence, *all tuples* of the relations in the FROM-clause are selected
- This is equivalent to the condition WHERE TRUE
- Query 9: Retrieve the SSN values for all employees.
```
Q9: SELECT SSN
    FROM EMPLOYEE
```
- If more than one relation is specified in the FROM-clause *and* there is no join condition, then the CARTESIAN PRODUCT of tuples is selected

- Example:
```
Q10: SELECT SSN, DNAME
     FROM EMPLOYEE, DEPARTMENT
```
- It is extremely important not to overlook specifying any selection and join conditions in the WHERE-clause; otherwise, incorrect and very large relations may result

# Use of *
- To retrive all the attribute values of the selected tuples, a `*` is used, which stands for *all the attributes*
- Example:
```
Q1C: SELECT *
     FROM EMPLOYEE
     WHERE DNO=5

Q1D: SELECT *
     FROM EMPLOYEE, DEPARTMENT
     WHERE DNAME='Research' AND DNO=DNUMBER
```

# Use of DISTINCT
- SQL does not treat a relation as a set; *duplicate tuples can appear*
- To eliminate duplicate tuples in a query result, the keyword **DISTINCT** is used
- For example, the result of Q11 may have duplicate SALARY values whereas Q11A does not have any duplicates
```
Q11: SELECT  SALARY
     FROM    EMPLOYEE

Q11A: SELECT   DISTINCT SALARY
      FROM     EMPLOYEE 
```

# Set Operations
- SQL has directly incorporated some set operations
- There is a union operation (**UNION**) and in *some version* of SQL there are set difference (**MINUS**) and intersection (**INTERSECT**) operations
- The resulting relations of these set operations are sets of tuples; *duplicate tuples are eliminated from the result*
- The set operations apply only to *union compatible relations*; the two relations must have the same attributes and the attributes must appear in the same order.

- Query 4: Make a list of all project names for project that involve an employee whose last name is 'Smith' as a worker or as a manager of the department that controls the project.
```
Q4: (SELECT PNAME
     FROM   PROJECT, DEPARTMENT, EMPLOYEE
     WHERE  DNUM=DNUMBER AND MGRSSN=SSN AND LNAME='Smith')
     UNION  (SELECT PNAME
		     FROM PROJECT, WORKS_ON, EMPLOYEE
		     WHERE PNUMBER=PNO AND ESSN=SSN AND LNAME='Smith')
```

# Nesting of Queries
- A complete SELECT query, called a *nested query*, can be specified within the WHERE-clause of another query, called *outer query*
- Many of the previous queries can be specified in an alternative from using nesting
- Query 1: Retrieve the name and address of the employees who work for the 'Research' department
```
Q1: SELECT FNAME, LNAME, ADDRESS
	FROM   EMPLOYEE
	WHERE  DNO IN (SELECT DNUMBER 
				   FROM DEPARTMENT 
				   WHERE DNAME='Research')
```

- The nested query select the number of the 'Research' department
- The outer query select an EMPLOYEE tuple if its DNO value is in the result of either nested query
- The comparison operator **IN** compares a value `v` with a set (or multi-set) of values `V`, and the evaluates to **TRUE** if `v` is one of the elements in `V`
- In general, we can have several levels of nested queries
- A reference to an *unqualified attribute* refers to the relation declared in the *innermost nested query*
- In this example, the nested query is *not correlated* with the other query.

## Correlated Nested Queries
- If a condition in the WHERE-clause of a *nested query* references an attribute of a relation declared in the *outer query*, the two queries are sai to be *correlated*
- The result of a correlated nested query id *different for each tuple* (or combination of tuples) of the relation(s) the outer query
- Query 12: Retrieve the name of each employee who has a dependent with the same first name as the employee
``` 
Q12: SELECT E.FNAME, E.LNAME
	 FROM   EMPLOYEE AS E
	 EHERE  E.SSN IN (SELECT ESSN
	                  FROM DEPENDENT
	                  WHERE ESSN=E.SSN AND E.FNAME=DEPENDENT_NAME)
```

- In Q12, the nested query has a different result *for each tuple* in the outer query
- A query written with nested SELECT... FROM... WHERE... blocks and using the = or IN comparison operators can **always** be expressed as a single block query. For example, Q12 may be written as in Q12A
```
Q12A: SELECT E.FNAME, E.LAME
      FROM   EMPLOYEE E, DEPENDENT D
      WHERE  E.SSN=D.ESSN AND E.FNAME=D.DEPENDENT_NAME
```

- The original SQL as specified for SYSTEM R also had **CONTAINS** comparison operator, which is used in conjunction with nested correlated queries
- This operator was dropped from the language, possibly of the difficulty in implementing efficiently

- Most implementations of SQL *do not* have this operator
- The CONTAINS operator compare two *set of values* and returns TRUE if one set contains all value in the other set (reminiscent of the division operation of algebra)
	- Query 3: Retrieve the name of each employee who works on all the project controlled by department number 5
```
Q3: SELECT FNAME, LNAME
    FROM   EMPLOYEE
    WHERE  ((SELECT PNO
		     FROM WORKS_ON
		     WHERE SSN=ESSN)
		     CONTAINS (SELECT PNUMBER
		     FROM PROJECT
		     WHERE DNUM=5))
```

- In Q3, the second nested query, which is not correlated with the outer query, retrieves the project numbers of all projects controlled by department 5
- The first nested query, which is correlated, retrieves the project numbers on which the employee works, which is different *for each employee tuple* because of the correlation

# The EXISTS Function
- EXISTS is used to check whether the result of a correlated nested query is empty (contains no tuples) or not
- We can formulate Query 12 in an alternative from that use EXISTS as Q12B below
- Query 12: Retrieve the name of each employee who has dependent with the same first name as the employee
```
Q12B: SELECT FNAME, LNAME
      FROM   EMPLOYEE
      WHEREEXISTS (SELECT *
			       FROM DEPENDENT
			       WHERE SSN=ESSN AND FNAME=DEPENDENT_NAME)
```

- Query 6: Retrieve the names of employees who have no dependents
```
Q6: SELECT FNAME, LNAME
	FROM   EMPLOYEE
	WHERE NOT EXISTS (SELECT * FROM DEPENDENT
					  WHERE SSN=ESSN)
```

- In Q6, the correlated nested query retrieves all DEPENDENT tuples related to an EMPLOYEE tuple. If *non exist*, the EMPLOYEE tuple is selected.
- EXISTS is necessary for the expressive power of SQL

# Explicit sets
- It is also possible to use **explicit (enumerated) set of values** in the WHERE-clause rather than a nested query
- Query 13: Retrieve the social security numbers of all employees who work on project number 1, 2 or 3.
```
Q13: SELECT DISTINCT ESSN
	 FROM WORKS_ON
	 WHERE PNO IN (1, 2, 3)
```

# NULLS in SQL queries
- SQL allows queries that check if a value is NULL (missing or undefined or not applicable)
- SQL uses **IS** or **IS NOT** to compare NULLs because it considers each NULL value distinct from other NULL values, so equality comparison is not appropriate
- Query 14: Retrieve the name of all employees who do not have supervisors.
```
Q14: SELECT FNAME, LNAME
	 FROM   EMPLOYEE
	 WHERE  SUPERSSN IS NULL
```
- Note: if a join condition is specified, tuples with NULL values for the join attributes are not included in the result

# Joined Relations Feature in SQL2
- Can specify a "joined relation" in the FROM-clause
- Looks like any other relation but is the result of a join
- Allows the user to specify different types of joins (regular "theta" JOIN, NATURAL JOIN, LEFT OUTER JOIN, RIGHT OUTER JOIN, CROSS JOIN, etc.)

- Example:
```
Q8: SELECT E.FNAME, E.LNAME, S.FNAME, S.LNAME
	FROM   EMPLOYEE E S
	WHERE  E.SUPERSSN=S.SSN
```
Can be written as:
```
Q8: SLECT E.FNAME, E.LNAME, S.FNAME, S.LNAME
	FROM (EMPLOYEE E LEFT OUTER JOIN EMPLOYEES ON E.SUPERSSN=E.SSN)
```

```
Q1: SLECT FNAME, LNAME, ADDRESS
	FROM  EMPLOYEE, DEPARTMENT
	WHERE DNAME='Research' AND DNUMBER=DNO
```
can be written as:
```
Q1: SELECT FNAME, LNAME, ADDRESS
	FROM   (EMPLOYEE JOIN DEPARTMENT ON NUMBER=DNO)
	WHERE  DNAME='Research'
```
or as:
```
Q1: SELECT FNAME, LNAME, ADDRESS
	FROM   (EMPLOYEE NATURAL JOIN DEPARTMENT AS DEPT(DNAME, DNO, MSSN, MSDATE))
	WHERE  DNAME='Research'
```

# Aggregate Functions
- Include **COUNT**, **SUM**, **MAX**, **MIN**, and **AVG**
- Query 15: Find the maximum salary, the minimum salary and the average salary among all employees.
```
Q15: SELECT MAX(SALARY), MIN(SALARY), AVG(SALARY)
	 FROM   EMPLOTEE
```

- Query 16: Find the maximum salary, the minimum salary and the average salary among all employees who work for the 'Research' department
```
Q16: SELECT MAX(SALARY), MIN(SALARY), AVG(SALARY)
	 FROM   EMPLOYEE, DEPARTMENT
	 EHERE  DNO=DNUMBER AND DNAME='Research'
```

- Query 17 and 18: Retrieve the total number of employees in the company (Q17) and the number of employees in the 'Research' department (Q18)

```
Q17: SELECT COUNT(*)
	 FROM   EMPLOYEE

Q18: SELECT COUNT(*)
	 FROM   EMPLOYEE, DEPARTMENT
	 WHERE  DNO=DNUMBER AND DNAME='Research'
```

# Grouping
- In many cases, we want to apply the aggregate functions to *subgroups of tuples in a relation*
- Each subgroup of tuples consist of the set of tuples that have *the same value* for the *grouping attribute(s)*
- The function is applied to each subgroup independently
- SQL has a **GROUP BY**-clause for specifying the grouping attributes, which must *must also appear in the SELECT-clause*

- Query 20: For each department, retrieve the department number, the number of employees in the department and their average salary
```
Q20: SELECT DNO, COUNT(*), AVG(SALARY)
	 FROM   EMPLOYEE
	 GROUP  BY DNO
```
- In Q20, the EMPLOYEE tuples are divided in two groups--each group having the same value for the grouping attribute DNO
- The COUNT and AVG functions are applied to each such group of tuples separately
- The SELECT-clause includes only the grouping attribute and the functions to be applied on each group of tuples
- A join condition can be used in conjunction grouping

- Query 21: For each project, retrieve the project number, project name and the number of employees who work on that project
``` 
Q21: SELECT   PNUMBER, PNAME, COUNT(*)
	 FROM     PROJECT, WORKS_ON
	 WHERE    PNUMBER=PNO
	 GROUP BY PNUMBER, PNAME
```
- In this case, the grouping and functions are applied *after* the joining of the relations

# INSERT
- Example: suppose we want to create a temporary table that has the name, number of employees and total salaries for each department. A table DEPTS_INFO is created by U3A and is loaded with the summary information retrieved from the database by the query in U3B
```
U3A: CREATE TABLE DEPTS_INFO
	 (DEPT_NAME  VARCHAR(10),
	  NO_OF_EMPS INTEGER,
	  TOTAL_SAL  INTEGER);

U3B: INSERTO  INTO DEPTS_INFO(DEPT_NAME, NO_OF_EMPS, TOTAL_SAL)
	 SELECT   DNAME, COUNT(*), SUM(SALARY)
	 FROM     DEPARTMENT, EMPLOYEE
	 WHERE    DNUMBER=DNO
	 GROUP BY DNAME;  
```
- Note: the DEPTS_INFO table may not be up-to-date if we change the tuples in either the DEPARTMENT or the EMPLOYEE relations *after* issuing U3B. We have to create a view to keep such a table up to date

# The HAVING-clause
- sometimes we want to retrieve the values of these functions for only those *groups that satisfy certain conditions*
- The HAVING-clause is used for specifying a selection condition on groups (rather than on individual tuples)

- Query 22: for each project on which more than two employees work, retrieve the project number, project name, and the number of employees who work on that project
```
Q22: SELECT   PNUMBER, PNAME, COUNT(*)
	 FROM     PROJECT, WORKS_ON
	 WHERE    PNUMBER=PNO
	 GROUP BY PNUMBER, PNAME
	 HAVING COUNT(*)>2
```

# Substring comparison
- the **LIKE** comparison operator is used to compare a partial strings
- Two reserved characters are used: '%' replaces an arbitrary number of characters, and '`_`' replaces a single arbitrary character

- Query 25: Retrieve all employees whose address is in Houston, Texas. Here, the value of the ADDRESS attribute must contains the substring 'Huston, TX'
```
Q25: SELECT FNAME, LNAME
	 FROM   EMPLOYEE
	 WHERE  ADDRESS LIKE '%Huston,TX'
```

# Arithmetic operations
- The standard arithmetic operators '+', '-', '`*`', and '/' can be applied to numeric values in an SQL query result
- Query 27: show the effect of giving all employees who work on the 'ProductX' project al 10% raise.
```
Q27: SELECT FNAME, LNAME, 1.1*SALARY
	 FROM   EMPLOYEE, WORKS_ON, PROJECT
	 WHERE  SSN=ESSN AND PNO=PNUMBER AND PNAME='ProjectX'
```

# Order By
- The **ORDER BY** clause is used to sort the tuples in a query result based on the values of some attribute(s)
- Query 28: Retrieve a list of employees and the projects each works in, ordered by the employee's department, and within each department ordered alphabetically by employee last name 
```
Q28: SELECT  DNAME, LNAME, FNAME, PNAME
	 FROM    DEPARTMENT, EMPLOYEE, WORKS_ON, PROJECT
	 WHERE   DNUMBER=DNO AND SSN=ESSN AND PNO=PNUMBER
	 ORDERED BY DNAME, LNAME
```
- The default order is ascending order of values
- We can specify the keyword **DESC** if we want a descending order; the keyword **ASC** can be used to explicitly specify ascending order, eve though it is the default

# Summary of SQL Queries
- A query in SQL can consist of up to six clauses, but only the first two, SELECT and FROM, are mandatory. The clauses are specified in the following order:
```
SELECT <attribute list>  
FROM <table list>  
[WHERE <condition>]  
[GROUP BY <grouping attribute(s)>]  
[HAVING <group condition>]  
[ORDER BY <attribute list>]
```

> [] $\rightarrow$ the command is optional
> example:
>  `[WHERE <condition>]` is optional
>  `SELECT <attribute list>` is not optional 

- The SELECT-clause lists the attributes or functions to be retrieved
- The FROM-clause specifies all relations (or aliases) needed in the query but not those needed in nested queries
- The WHERE-clause specifies the conditions for selection and join of tuples from the relations specified in the FROM-clause
- GROUP BY specifies grouping attributes
- HAVING specifies a condition for selection of groups
- ORDER BY specifies an order for displaying the result of a query
- A query is evaluated by first applying the WHERE-clause, then GROUP BY and HAVING, and finally the SELECT-clause




