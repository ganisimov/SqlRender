library("testthat")

test_that("translateSQL sql server -> Oracle DATEDIFF", {
  sql <- translateSql("SELECT DATEDIFF(dd,drug_era_start_date,drug_era_end_date) FROM drug_era;",
                      sourceDialect = "sql server",
                      targetDialect = "oracle")$sql
  expect_equal(sql, "SELECT (drug_era_end_date - drug_era_start_date) FROM drug_era;")
})


test_that("translateSQL sql server -> Oracle DATEADD", {
  sql <- translateSql("SELECT DATEADD(dd,30,drug_era_end_date) FROM drug_era;",
                      sourceDialect = "sql server",
                      targetDialect = "oracle")$sql
  expect_equal(sql, "SELECT (drug_era_end_date + 30) FROM drug_era;")
})

test_that("translateSQL sql server -> Oracle USE", {
  sql <- translateSql("USE vocabulary;", sourceDialect = "sql server", targetDialect = "oracle")$sql
  expect_equal(sql, "ALTER SESSION SET current_schema = vocabulary;")
})

test_that("translateSQL sql server -> Oracle DROP TABLE IF EXISTS", {
  sql <- translateSql("IF OBJECT_ID('cohort', 'U') IS NOT NULL DROP TABLE cohort;",
                      sourceDialect = "sql server",
                      targetDialect = "oracle")$sql
  expect_equal(sql,
               "BEGIN\n  EXECUTE IMMEDIATE 'TRUNCATE TABLE cohort';\n  EXECUTE IMMEDIATE 'DROP TABLE cohort';\nEXCEPTION\n  WHEN OTHERS THEN\n    IF SQLCODE != -942 THEN\n      RAISE;\n    END IF;\nEND;")
})


test_that("translateSQL sql server -> Oracle CAST(AS DATE)", {
  sql <- translateSql("CAST('20000101' AS DATE);",
                      sourceDialect = "sql server",
                      targetDialect = "oracle")$sql
  expect_equal(sql, "TO_DATE('20000101', 'yyyymmdd');")
})

test_that("translateSQL sql server -> Oracle concatenate string operator", {
  sql <- translateSql("select distinct cast(cast(YEAR(observation_period_start_date) as varchar(4)) +  '01' + '01' as date) as obs_year;",
                      sourceDialect = "sql server",
                      targetDialect = "oracle")$sql
  expect_equal(sql,
               "SELECT distinct TO_DATE(TO_CHAR(EXTRACT(YEAR FROM observation_period_start_date)) || '01' || '01', 'yyyymmdd') as obs_year FROM DUAL;")
})

test_that("translateSQL sql server -> Oracle RIGHT functions", {
  sql <- translateSql("select RIGHT(x,4);",
                      sourceDialect = "sql server",
                      targetDialect = "oracle")$sql
  expect_equal(sql, "SELECT SUBSTR(x,-4) FROM DUAL;")
})

test_that("translateSQL sql server -> Oracle complex query", {
  sql <- translateSql("select CAST(CAST(YEAR(x) AS VARCHAR(12)) + RIGHT('0'+MONTH(x),2) + '01' AS DATE);",
                      sourceDialect = "sql server",
                      targetDialect = "oracle")$sql
  expect_equal(sql,
               "SELECT TO_DATE(TO_CHAR(EXTRACT(YEAR FROM x)) || SUBSTR('0' ||EXTRACT(MONTH FROM x),-2) || '01', 'yyyymmdd') FROM DUAL;")
})

test_that("translateSQL sql server -> Oracle '+' in quote", {
  sql <- translateSql("select '+';", sourceDialect = "sql server", targetDialect = "oracle")$sql
  expect_equal(sql, "SELECT '+' FROM DUAL;")
})

test_that("translateSQL sql server -> PostgreSQL USE", {
  sql <- translateSql("USE vocabulary;",
                      sourceDialect = "sql server",
                      targetDialect = "postgresql")$sql
  expect_equal(sql, "SET search_path TO vocabulary;")
})

test_that("translateSQL sql server -> PostgreSQL string concat", {
  sql <- translateSql("'x' + b ( 'x' + b)",
                      sourceDialect = "sql server",
                      targetDialect = "postgresql")$sql
  expect_equal(sql, "'x' || b ( 'x' || b)")
})

test_that("translateSQL sql server -> PostgreSQL string concat", {
  sql <- translateSql("a + ';b'", sourceDialect = "sql server", targetDialect = "postgresql")$sql
  expect_equal(sql, "a || ';b'")
})

test_that("translateSQL sql server -> PostgreSQL string concat", {
  sql <- translateSql("a + ';('", sourceDialect = "sql server", targetDialect = "postgresql")$sql
  expect_equal(sql, "a || ';('")
})


test_that("translateSQL sql server -> PostgreSQL add month", {
  sql <- translateSql("DATEADD(mm,1,date)",
                      sourceDialect = "sql server",
                      targetDialect = "postgresql")$sql
  expect_equal(sql, "CAST((date + 1*INTERVAL'1 month') AS DATE)")
})

test_that("translateSQL sql server -> Oracle multiple inserts in one statement", {
  sql <- translateSql("INSERT INTO my_table (key,value) VALUES (1,0),(2,0),(3,1)",
                      sourceDialect = "sql server",
                      targetDialect = "oracle")$sql
  expect_equal(sql,
               "INSERT ALL\nINTO my_table (key,value) VALUES (1,0)\n INTO my_table (key,value) VALUES (2,0)\n)\n INTO my_table (key,value) VALUES (3,1)\nSELECT * FROM dual")
})

test_that("translateSQL sql server -> RedShift VARCHAR(MAX)", {
  sql <- translateSql("VARCHAR(MAX)", sourceDialect = "sql server", targetDialect = "redshift")$sql
  expect_equal(sql, "VARCHAR(MAX)")
})

test_that("translateSQL sql server -> Postgres WITH SELECT", {
  sql <- translateSql("WITH cte1 AS (SELECT a FROM b) SELECT c FROM cte1;",
                      sourceDialect = "sql server",
                      targetDialect = "postgresql")$sql
  expect_equal(sql, "WITH cte1 AS (SELECT a FROM b) SELECT c FROM cte1;")
})

test_that("translateSQL sql server -> Postgres WITH SELECT INTO", {
  sql <- translateSql("WITH cte1 AS (SELECT a FROM b) SELECT c INTO d FROM cte1;",
                      sourceDialect = "sql server",
                      targetDialect = "postgresql")$sql
  expect_equal(sql,
               "CREATE TABLE d\nAS\nWITH cte1 AS (SELECT a FROM b) SELECT\nc\nFROM\ncte1;")
})

test_that("translateSQL sql server -> Postgres WITH SELECT INTO without FROM", {
  sql <- translateSql("SELECT c INTO d;",
                      sourceDialect = "sql server",
                      targetDialect = "postgresql")$sql
  expect_equal(sql, "CREATE TABLE d AS\nSELECT\nc;")
})


test_that("translateSQL sql server -> Postgres WITH INSERT INTO SELECT", {
  sql <- translateSql("WITH cte1 AS (SELECT a FROM b) INSERT INTO c (d int) SELECT e FROM cte1;",
                      sourceDialect = "sql server",
                      targetDialect = "postgresql")$sql
  expect_equal(sql, "WITH cte1 AS (SELECT a FROM b) INSERT INTO c (d int) SELECT e FROM cte1;")
})

test_that("translateSQL sql server -> Oracle WITH SELECT", {
  sql <- translateSql("WITH cte1 AS (SELECT a FROM b) SELECT c FROM cte1;",
                      sourceDialect = "sql server",
                      targetDialect = "oracle")$sql
  expect_equal(sql, "WITH cte1 AS (SELECT a FROM b) SELECT c FROM cte1;")
})

test_that("translateSQL sql server -> Oracle WITH SELECT INTO", {
  sql <- translateSql("WITH cte1 AS (SELECT a FROM b) SELECT c INTO d FROM cte1;",
                      sourceDialect = "sql server",
                      targetDialect = "oracle")$sql
  expect_equal(sql,
               "CREATE TABLE d\nAS\nWITH cte1 AS (SELECT a FROM b) SELECT\nc\nFROM\ncte1;")
})

test_that("translateSQL sql server -> Oracle WITH INSERT INTO SELECT", {
  sql <- translateSql("WITH cte1 AS (SELECT a FROM b) INSERT INTO c (d int) SELECT e FROM cte1;",
                      sourceDialect = "sql server",
                      targetDialect = "oracle")$sql
  expect_equal(sql,
               "INSERT INTO c (d int) WITH cte1 AS (SELECT a FROM b) SELECT e FROM cte1;")
})

test_that("translateSQL sql server -> PDW WITH SELECT INTO", {
  sql <- translateSql("WITH cte1 AS (SELECT a FROM b) SELECT c INTO d FROM cte1;",
                      sourceDialect = "sql server",
                      targetDialect = "pdw")$sql
  expect_equal(sql,
               "IF XACT_STATE() = 1 COMMIT; CREATE TABLE  d WITH (DISTRIBUTION = REPLICATE)\nAS\nWITH cte1 AS (SELECT a FROM b) SELECT\nc\nFROM\ncte1;")
})

test_that("translateSQL sql server -> PDW WITH SELECT INTO temp table", {
  sql <- translateSql("WITH cte1 AS (SELECT a FROM b) SELECT c INTO #d FROM cte1;",
                      sourceDialect = "sql server",
                      targetDialect = "pdw")$sql
  expect_equal(sql,
               "IF XACT_STATE() = 1 COMMIT; CREATE TABLE  #d WITH (LOCATION = USER_DB, DISTRIBUTION = REPLICATE) AS\nWITH cte1 AS (SELECT a FROM b) SELECT\nc\nFROM\ncte1;")
})

test_that("translateSQL sql server -> PDW create temp table", {
  sql <- translateSql("CREATE TABLE #a (x int);",
                      sourceDialect = "sql server",
                      targetDialect = "pdw")$sql
  expect_equal(sql,
               "IF XACT_STATE() = 1 COMMIT; CREATE TABLE  #a (x int)\nWITH (LOCATION = USER_DB, DISTRIBUTION = REPLICATE);")
})

test_that("translateSQL sql server -> PDW create temp table with person_id", {
  sql <- translateSql("CREATE TABLE #a (person_id int);",
                      sourceDialect = "sql server",
                      targetDialect = "pdw")$sql
  expect_equal(sql,
               "IF XACT_STATE() = 1 COMMIT; CREATE TABLE  #a (person_id int)\nWITH (LOCATION = USER_DB, DISTRIBUTION = HASH(person_id));")
})

test_that("translateSQL sql server -> PDW create temp table with subject_id", {
  sql <- translateSql("CREATE TABLE #a (subject_id int);",
                      sourceDialect = "sql server",
                      targetDialect = "pdw")$sql
  expect_equal(sql,
               "IF XACT_STATE() = 1 COMMIT; CREATE TABLE  #a (subject_id int)\nWITH (LOCATION = USER_DB, DISTRIBUTION = HASH(subject_id));")
})

test_that("translateSQL sql server -> PDW create temp table with analysis_id", {
  sql <- translateSql("CREATE TABLE #a (analysis_id int);",
                      sourceDialect = "sql server",
                      targetDialect = "pdw")$sql
  expect_equal(sql,
               "IF XACT_STATE() = 1 COMMIT; CREATE TABLE  #a (analysis_id int)\nWITH (LOCATION = USER_DB, DISTRIBUTION = HASH(analysis_id));")
})

test_that("translateSQL sql server -> PDW create permanent table", {
  sql <- translateSql("CREATE TABLE a (x int);",
                      sourceDialect = "sql server",
                      targetDialect = "pdw")$sql
  expect_equal(sql,
               "IF XACT_STATE() = 1 COMMIT; CREATE TABLE  a (x int)\nWITH (DISTRIBUTION = REPLICATE);")
})

test_that("translateSQL sql server -> PDW create permanent table with person_id", {
  sql <- translateSql("CREATE TABLE a (person_id int);",
                      sourceDialect = "sql server",
                      targetDialect = "pdw")$sql
  expect_equal(sql,
               "IF XACT_STATE() = 1 COMMIT; CREATE TABLE  a ( person_id int)\nWITH (DISTRIBUTION = HASH(person_id));")
})

test_that("translateSQL sql server -> PDW create permanent table with subject_id", {
  sql <- translateSql("CREATE TABLE a (subject_id int);",
                      sourceDialect = "sql server",
                      targetDialect = "pdw")$sql
  expect_equal(sql,
               "IF XACT_STATE() = 1 COMMIT; CREATE TABLE  a ( subject_id int)\nWITH (DISTRIBUTION = HASH(subject_id));")
})

test_that("translateSQL sql server -> PDW create permanent table with analysis_id", {
  sql <- translateSql("CREATE TABLE a (analysis_id int);",
                      sourceDialect = "sql server",
                      targetDialect = "pdw")$sql
  expect_equal(sql,
               "IF XACT_STATE() = 1 COMMIT; CREATE TABLE  a ( analysis_id int)\nWITH (DISTRIBUTION = HASH(analysis_id));")
})

test_that("translateSQL sql server -> PDW select into permanent table", {
  sql <- translateSql("SELECT a INTO b FROM c WHERE a = 1;",
                      sourceDialect = "sql server",
                      targetDialect = "pdw")$sql
  expect_equal(sql,
               "IF XACT_STATE() = 1 COMMIT; CREATE TABLE  b WITH (DISTRIBUTION = REPLICATE)\nAS\nSELECT\na\nFROM\nc WHERE a = 1;")
})

test_that("translateSQL sql server -> PDW select into permanent table with person_id", {
  sql <- translateSql("SELECT a, person_id, b INTO b FROM c WHERE a = 1;",
                      sourceDialect = "sql server",
                      targetDialect = "pdw")$sql
  expect_equal(sql,
               "IF XACT_STATE() = 1 COMMIT; CREATE TABLE  b WITH (DISTRIBUTION = HASH(person_id))\nAS\nSELECT\na, person_id, b\nFROM\nc WHERE a = 1;")
})

test_that("translateSQL sql server -> PDW select into permanent table with analysis_id", {
  sql <- translateSql("SELECT a, analysis_id, b INTO b FROM c WHERE a = 1;",
                      sourceDialect = "sql server",
                      targetDialect = "pdw")$sql
  expect_equal(sql,
               "IF XACT_STATE() = 1 COMMIT; CREATE TABLE  b WITH (DISTRIBUTION = HASH(analysis_id))\nAS\nSELECT\na, analysis_id, b\nFROM\nc WHERE a = 1;")
})

test_that("translateSQL sql server -> Postgres create table if not exists", {
  sql <- translateSql("IF OBJECT_ID('cohort', 'U') IS NULL\n CREATE TABLE cohort\n(cohort_definition_id INT);",
                      sourceDialect = "sql server",
                      targetDialect = "postgresql")$sql
  expect_equal(sql, "CREATE TABLE IF NOT EXISTS cohort\n (cohort_definition_id INT);")
})

test_that("translateSQL sql server -> Redshift create table if not exists", {
  sql <- translateSql("IF OBJECT_ID('cohort', 'U') IS NULL\n CREATE TABLE cohort\n(cohort_definition_id INT);",
                      sourceDialect = "sql server",
                      targetDialect = "redshift")$sql
  expect_equal(sql, "CREATE TABLE IF NOT EXISTS cohort\n (cohort_definition_id INT);")
})

test_that("translateSQL sql server -> Oracle create table if not exists", {
  sql <- translateSql("IF OBJECT_ID('cohort', 'U') IS NULL\n CREATE TABLE cohort\n(cohort_definition_id INT);",
                      sourceDialect = "sql server",
                      targetDialect = "oracle")$sql
  expect_equal(sql,
               "BEGIN\n  EXECUTE IMMEDIATE 'CREATE TABLE cohort\n (cohort_definition_id INT)';\nEXCEPTION\n  WHEN OTHERS THEN\n    IF SQLCODE != -955 THEN\n      RAISE;\n    END IF;\nEND;")
})

test_that("translateSQL sql server -> Oracle datefromparts", {
  sql <- translateSql("SELECT DATEFROMPARTS(year,month,day) FROM table",
                      sourceDialect = "sql server",
                      targetDialect = "oracle")$sql
  expect_equal(sql,
               "SELECT TO_DATE(TO_CHAR(year,'0000')||'-'||TO_CHAR(month,'00')||'-'||TO_CHAR(day,'00'), 'YYYY-MM-DD') FROM table")
})

test_that("translateSQL sql server -> redshift datefromparts", {
  sql <- translateSql("SELECT DATEFROMPARTS(year,month,day) FROM table",
                      sourceDialect = "sql server",
                      targetDialect = "redshift")$sql
  expect_equal(sql,
               "SELECT TO_DATE(TO_CHAR(year,'0000FM')||'-'||TO_CHAR(month,'00FM')||'-'||TO_CHAR(day,'00FM'), 'YYYY-MM-DD') FROM table")
})


test_that("translateSQL sql server -> Oracle datetime to timestamp", {
  sql <- translateSql("CREATE TABLE x (a DATETIME)",
                      sourceDialect = "sql server",
                      targetDialect = "oracle")$sql
  expect_equal(sql, "CREATE TABLE x (a TIMESTAMP)")
})

test_that("translateSQL sql server -> Oracle select random row", {
  sql <- translateSql("SELECT column FROM (SELECT column, ROW_NUMBER() OVER (ORDER BY RAND()) AS rn FROM table) tmp WHERE rn <= 1",
                      sourceDialect = "sql server",
                      targetDialect = "oracle")$sql
  expect_equal(sql,
               "SELECT column FROM (SELECT column, ROW_NUMBER() OVER (ORDER BY DBMS_RANDOM.VALUE) AS rn FROM table) tmp WHERE rn <= 1")
})

test_that("translateSQL sql server -> Postgres select random row", {
  sql <- translateSql("SELECT column FROM (SELECT column, ROW_NUMBER() OVER (ORDER BY RAND()) AS rn FROM table) tmp WHERE rn <= 1",
                      sourceDialect = "sql server",
                      targetDialect = "postgresql")$sql
  expect_equal(sql,
               "SELECT column FROM (SELECT column, ROW_NUMBER() OVER (ORDER BY RANDOM()) AS rn FROM table) tmp WHERE rn <= 1")
})

test_that("translateSQL sql server -> Redshift select random row", {
  sql <- translateSql("SELECT column FROM (SELECT column, ROW_NUMBER() OVER (ORDER BY RAND()) AS rn FROM table) tmp WHERE rn <= 1",
                      sourceDialect = "sql server",
                      targetDialect = "redshift")$sql
  expect_equal(sql,
               "SELECT column FROM (SELECT column, ROW_NUMBER() OVER (ORDER BY RANDOM()) AS rn FROM table) tmp WHERE rn <= 1")
})

test_that("translateSQL sql server -> Oracle select random row using hash", {
  sql <- translateSql("SELECT column FROM (SELECT column, ROW_NUMBER() OVER (ORDER BY HASHBYTES('MD5',CAST(person_id AS varchar))) tmp WHERE rn <= 1",
                      sourceDialect = "sql server",
                      targetDialect = "oracle")$sql
  expect_equal(sql,
               "SELECT column FROM (SELECT column, ROW_NUMBER() OVER (ORDER BY DBMS_CRYPTO.HASH(TO_CHAR(person_id),2)) tmp WHERE rn <= 1")
})

test_that("translateSQL sql server -> Postgres select random row using hash", {
  sql <- translateSql("SELECT column FROM (SELECT column, ROW_NUMBER() OVER (ORDER BY HASHBYTES('MD5',CAST(person_id AS varchar))) tmp WHERE rn <= 1",
                      sourceDialect = "sql server",
                      targetDialect = "postgresql")$sql
  expect_equal(sql,
               "SELECT column FROM (SELECT column, ROW_NUMBER() OVER (ORDER BY MD5(CAST(person_id AS varchar))) tmp WHERE rn <= 1")
})

test_that("translateSQL sql server -> Redshift select random row using hash", {
  sql <- translateSql("SELECT column FROM (SELECT column, ROW_NUMBER() OVER (ORDER BY HASHBYTES('MD5',CAST(person_id AS varchar))) tmp WHERE rn <= 1",
                      sourceDialect = "sql server",
                      targetDialect = "redshift")$sql
  expect_equal(sql,
               "SELECT column FROM (SELECT column, ROW_NUMBER() OVER (ORDER BY MD5(CAST(person_id AS varchar))) tmp WHERE rn <= 1")
})

test_that("translateSQL sql server -> PDW cte with preceding 'with' in quotes", {
  sql <- translateSql("insert into x (a) values ('with'); with cte (a) as(select a from b) select a INTO #c from cte;",
                      sourceDialect = "sql server",
                      targetDialect = "pdw")$sql
  expect_equal(sql,
               "insert into x (a) values ('with'); IF XACT_STATE() = 1 COMMIT; CREATE TABLE  #c WITH (LOCATION = USER_DB, DISTRIBUTION = REPLICATE) AS\nWITH cte (a) AS (select a from b) SELECT\na\nFROM\ncte;")
})

test_that("translateSQL sql server throws error when invalid target is given", {
  expect_error(translateSql("iSELECT * FROM a;", targetDialect = "pwd")$sql)
})


test_that("translateSQL select into issue for pdw", {
  sql <- "SELECT @c1 INTO table FROM @c2 WHERE a = 1;"
  sql <- translateSql(sql, targetDialect = "pdw")$sql
  expect_equal(sql, "IF XACT_STATE() = 1 COMMIT; CREATE TABLE  table WITH (DISTRIBUTION = REPLICATE)\nAS\nSELECT\n@c1\nFROM\n@c2 WHERE a = 1;")
})



test_that("translateSQL ## issue on oracle", {
  sql <- "SELECT a FROM c##blah.table;"
  sql <- translateSql(sql, targetDialect = "oracle")$sql
  expect_equal(sql, "SELECT a FROM c##blah.table;")
})

# Impala tests

test_that("translateSQL sql server -> Impala USE", {
    sql <- translateSql("USE vocabulary;",
    sourceDialect = "sql server",
    targetDialect = "impala")$sql
    expect_equal(sql, "USE vocabulary;")
})

test_that("translateSQL sql server -> Impala CAST(AS DATE)", {
    sql <- translateSql("CAST('20000101' AS DATE);",
    sourceDialect = "sql server",
    targetDialect = "impala")$sql
    expect_equal(sql, "CASE TYPEOF('20000101') WHEN 'TIMESTAMP' THEN CAST('20000101' AS TIMESTAMP) ELSE TO_UTC_TIMESTAMP(CONCAT_WS('-', SUBSTR(CAST('20000101' AS STRING), 1, 4), SUBSTR(CAST('20000101' AS STRING), 5, 2), SUBSTR(CAST('20000101' AS STRING), 7, 2)), 'UTC') END;")
})

test_that("translateSQL sql server -> Impala DATEDIFF", {
    sql <- translateSql("SELECT DATEDIFF(dd,drug_era_start_date,drug_era_end_date) FROM drug_era;",
    sourceDialect = "sql server",
    targetDialect = "impala")$sql
    expect_equal(sql, "SELECT DATEDIFF(CASE TYPEOF(drug_era_end_date) WHEN 'TIMESTAMP' THEN CAST(drug_era_end_date AS TIMESTAMP) ELSE TO_UTC_TIMESTAMP(CONCAT_WS('-', SUBSTR(CAST(drug_era_end_date AS STRING), 1, 4), SUBSTR(CAST(drug_era_end_date AS STRING), 5, 2), SUBSTR(CAST(drug_era_end_date AS STRING), 7, 2)), 'UTC') END, CASE TYPEOF(drug_era_start_date) WHEN 'TIMESTAMP' THEN CAST(drug_era_start_date AS TIMESTAMP) ELSE TO_UTC_TIMESTAMP(CONCAT_WS('-', SUBSTR(CAST(drug_era_start_date AS STRING), 1, 4), SUBSTR(CAST(drug_era_start_date AS STRING), 5, 2), SUBSTR(CAST(drug_era_start_date AS STRING), 7, 2)), 'UTC') END) FROM drug_era;")
})

test_that("translateSQL sql server -> Impala DATEADD", {
    sql <- translateSql("SELECT DATEADD(dd,30,drug_era_end_date) FROM drug_era;",
    sourceDialect = "sql server",
    targetDialect = "impala")$sql
    expect_equal(sql, "SELECT DATE_ADD(CASE TYPEOF(drug_era_end_date) WHEN 'TIMESTAMP' THEN CAST(drug_era_end_date AS TIMESTAMP) ELSE TO_UTC_TIMESTAMP(CONCAT_WS('-', SUBSTR(CAST(drug_era_end_date AS STRING), 1, 4), SUBSTR(CAST(drug_era_end_date AS STRING), 5, 2), SUBSTR(CAST(drug_era_end_date AS STRING), 7, 2)), 'UTC') END, 30) FROM drug_era;")
})

test_that("translateSQL sql server -> Impala WITH SELECT", {
    sql <- translateSql("WITH cte1 AS (SELECT a FROM b) SELECT c FROM cte1;",
    sourceDialect = "sql server",
    targetDialect = "impala")$sql
    expect_equal(sql, "WITH cte1 AS (SELECT a FROM b) SELECT c FROM cte1;")
})

test_that("translateSQL sql server -> Impala WITH SELECT INTO", {
    sql <- translateSql("WITH cte1 AS (SELECT a FROM b) SELECT c INTO d FROM cte1;",
    sourceDialect = "sql server",
    targetDialect = "impala")$sql
    expect_equal(sql,
    "CREATE TABLE d\nAS\nWITH cte1 AS (SELECT a FROM b) SELECT\nc\nFROM\ncte1;")
})

test_that("translateSQL sql server -> Impala WITH SELECT INTO without FROM", {
    sql <- translateSql("SELECT c INTO d;",
    sourceDialect = "sql server",
    targetDialect = "impala")$sql
    expect_equal(sql, "CREATE TABLE d AS\nSELECT\nc;")
})

test_that("translateSQL sql server -> Impala create table if not exists", {
    sql <- translateSql("IF OBJECT_ID('cohort', 'U') IS NULL\n CREATE TABLE cohort\n(cohort_definition_id INT);",
    sourceDialect = "sql server",
    targetDialect = "impala")$sql
    expect_equal(sql, "CREATE TABLE IF NOT EXISTS cohort\n (cohort_definition_id INT);")
})

test_that("translateSQL sql server -> Impala DROP TABLE IF EXISTS", {
    sql <- translateSql("IF OBJECT_ID('cohort', 'U') IS NOT NULL DROP TABLE cohort;",
    sourceDialect = "sql server",
    targetDialect = "impala")$sql
    expect_equal(sql,
    "DROP TABLE IF EXISTS cohort;")
})

test_that("translateSQL sql server -> Impala RIGHT functions", {
    sql <- translateSql("SELECT RIGHT(x,4);",
    sourceDialect = "sql server",
    targetDialect = "impala")$sql
    expect_equal(sql, "SELECT SUBSTR(x,-4);")
})

test_that("translateSQL sql server -> Impala DELETE FROM", {
    sql <- translateSql("delete from ACHILLES_results;",
    sourceDialect = "sql server",
    targetDialect = "impala")$sql
    expect_equal(sql, "/* DELETE FROM ACHILLES_results; */")
})

test_that("translateSQL sql server -> Impala DELETE FROM WHERE", {
    sql <- translateSql("delete from ACHILLES_results where analysis_id IN (1, 2, 3);",
    sourceDialect = "sql server",
    targetDialect = "impala")$sql
    expect_equal(sql, "/* DELETE FROM ACHILLES_results where analysis_id IN (1, 2, 3); */")
})

test_that("translateSQL sql server -> Impala location reserved word", {
    sql <- translateSql("select count(1) from omop_cdm.location;",
    sourceDialect = "sql server",
    targetDialect = "impala")$sql
    expect_equal(sql, "select count(1) from omop_cdm.`location`;")
})


# Netezza tests

test_that("translateSQL sql server -> Netezza CAST(AS DATE)", {
    sql <- translateSql("CAST('20000101' AS DATE);",
    sourceDialect = "sql server",
    targetDialect = "netezza")$sql
    expect_equal(sql, "TO_DATE('20000101', 'yyyymmdd');")
})

test_that("translateSQL sql server -> Netezza DATEDIFF", {
    sql <- translateSql("SELECT DATEDIFF(dd,drug_era_start_date,drug_era_end_date) FROM drug_era;",
    sourceDialect = "sql server",
    targetDialect = "netezza")$sql
    expect_equal(sql, "SELECT (CAST(drug_era_end_date AS DATE) - CAST(drug_era_start_date AS DATE)) FROM drug_era;")
})

test_that("translateSQL sql server -> Netezza DATEADD", {
    sql <- translateSql("SELECT DATEADD(dd,30,drug_era_end_date) FROM drug_era;",
    sourceDialect = "sql server",
    targetDialect = "netezza")$sql
    expect_equal(sql, "SELECT (drug_era_end_date + 30) FROM drug_era;")
})

test_that("translateSQL sql server -> Netezza WITH SELECT", {
    sql <- translateSql("WITH cte1 AS (SELECT a FROM b) SELECT c FROM cte1;",
    sourceDialect = "sql server",
    targetDialect = "netezza")$sql
    expect_equal(sql, "WITH cte1 AS (SELECT a FROM b) SELECT c FROM cte1;")
})

test_that("translateSQL sql server -> Netezza WITH SELECT INTO", {
    sql <- translateSql("WITH cte1 AS (SELECT a FROM b) SELECT c INTO d FROM cte1;",
    sourceDialect = "sql server",
    targetDialect = "netezza")$sql
    expect_equal(sql,
    "CREATE TABLE d\nAS\nWITH cte1 AS (SELECT a FROM b) SELECT\nc\nFROM\ncte1;")
})

test_that("translateSQL sql server -> Netezza DROP TABLE IF EXISTS", {
    sql <- translateSql("IF OBJECT_ID('cohort', 'U') IS NOT NULL DROP TABLE cohort;",
    sourceDialect = "sql server",
    targetDialect = "netezza")$sql
    expect_equal(sql,
    "DROP TABLE cohort IF EXISTS;")
})

test_that("translateSQL sql server -> Netezza RIGHT functions", {
    sql <- translateSql("SELECT RIGHT(x,4);",
    sourceDialect = "sql server",
    targetDialect = "netezza")$sql
    expect_equal(sql, "SELECT STRRIGHT(x,4);")
})

test_that("translateSQL sql server -> Netezza DELETE FROM WHERE", {
    sql <- translateSql("delete from ACHILLES_results where analysis_id IN (1, 2, 3);",
    sourceDialect = "sql server",
    targetDialect = "netezza")$sql
    expect_equal(sql, "delete from ACHILLES_results where analysis_id IN (1, 2, 3);")
})

test_that("translateSQL sql server -> Netezza CAST AS VARCHAR", {
    sql <- translateSql("CAST(person_id AS VARCHAR);",
    sourceDialect = "sql server",
    targetDialect = "netezza")$sql
    expect_equal(sql, "CAST(person_id AS VARCHAR(1000));")
})

test_that("translateSQL sql server -> pdw doesn't translate string literal", {
    sql <- translateSql("SELECT 'CREATE TABLE table (id int not null);'",
    sourceDialect = "sql server",
    targetDialect = "pdw")$sql
    expect_equal(sql, "SELECT 'CREATE TABLE table (id int not null);'")

    sql <- translateSql("CREATE TABLE table (id int not null);",
    sourceDialect = "sql server",
    targetDialect = "pdw")$sql
    expect_equal(sql, "IF XACT_STATE() = 1 COMMIT; CREATE TABLE  table (id int not null)\nWITH (DISTRIBUTION = REPLICATE);")
})

test_that("translateSQL sql server -> redshift DISTINCT + TOP", {
    sql <- translateSql("SELECT distinct top 1000 a from table;",
    sourceDialect = "sql server",
    targetDialect = "redshift")$sql
    expect_equal(sql, "SELECT TOP 1000 DISTINCT a from table;")
})

test_that("translateSQL sql server -> RedShift DATE ADD DAYS dd", {
  sql <- translateSql("SELECT DATEADD(dd,30,drug_era_end_date) FROM drug_era;", sourceDialect = "sql server", targetDialect = "redshift")$sql
  expect_equal(sql, "SELECT DATEADD(day, 30, drug_era_end_date) FROM drug_era;")
})

test_that("translateSQL sql server -> RedShift DATE ADD MONTH mm", {
  sql <- translateSql("SELECT DATEADD(mm,3,drug_era_end_date) FROM drug_era;", sourceDialect = "sql server", targetDialect = "redshift")$sql
  expect_equal(sql, "SELECT DATEADD(month, 3, drug_era_end_date) FROM drug_era;")
})

test_that("translateSQL sql server -> RedShift DATE ADD MONTH m", {
  sql <- translateSql("SELECT DATEADD(m,3,drug_era_end_date) FROM drug_era;", sourceDialect = "sql server", targetDialect = "redshift")$sql
  expect_equal(sql, "SELECT DATEADD(month, 3, drug_era_end_date) FROM drug_era;")
})

test_that("translateSQL sql server -> RedShift DATE ADD YEAR yyyy", {
  sql <- translateSql("SELECT DATEADD(yyyy,3,drug_era_end_date) FROM drug_era;", sourceDialect = "sql server", targetDialect = "redshift")$sql
  expect_equal(sql, "SELECT DATEADD(year, 3, drug_era_end_date) FROM drug_era;")
})

test_that("translateSQL sql server -> RedShift DATE ADD YEAR yy", {
  sql <- translateSql("SELECT DATEADD(yy,3,drug_era_end_date) FROM drug_era;", sourceDialect = "sql server", targetDialect = "redshift")$sql
  expect_equal(sql, "SELECT DATEADD(year, 3, drug_era_end_date) FROM drug_era;")
})

test_that("translateSQL sql server -> RedShift DATE DIFF dd", {
  sql <- translateSql("SELECT DATEDIFF(dd,drug_era_start_date,drug_era_end_date) FROM drug_era;", sourceDialect = "sql server", targetDialect = "redshift")$sql
  expect_equal(sql, "SELECT DATEDIFF(day, drug_era_start_date, drug_era_end_date) FROM drug_era;")
})

test_that("translateSQL sql server -> RedShift DATETIMEFROMPARTS", {
  sql <- translateSql("SELECT DATETIMEFROMPARTS(year,month,day,hour,minute,second,millisecond) FROM table", sourceDialect = "sql server", targetDialect = "redshift")$sql
  expect_equal(sql, "SELECT CAST(TO_CHAR(year,'0000FM')||'-'||TO_CHAR(month,'00FM')||'-'||TO_CHAR(day,'00FM')||' '||TO_CHAR(hour,'00FM')||':'||TO_CHAR(minute,'00FM')||':'||TO_CHAR(second,'00FM')||'.'||TO_CHAR(millisecond,'000FM') as TIMESTAMP) FROM table")
})

test_that("translateSQL sql server -> RedShift EOMONTH", {
  sql <- translateSql("SELECT EOMONTH(date) FROM table", sourceDialect = "sql server", targetDialect = "redshift")$sql
  expect_equal(sql, "SELECT LAST_DAY(date) FROM table")
})

test_that("translateSQL sql server -> RedShift VARIANCE", {
  sql <- translateSql("SELECT VAR(a) FROM table", sourceDialect = "sql server", targetDialect = "redshift")$sql
  expect_equal(sql, "SELECT VARIANCE(a) FROM table")
})

test_that("translateSQL sql server -> RedShift SQUARE", {
  sql <- translateSql("SELECT SQUARE(a + b) FROM table", sourceDialect = "sql server", targetDialect = "redshift")$sql
  expect_equal(sql, "SELECT ((a + b) * (a + b)) FROM table")
})

test_that("translateSQL sql server -> RedShift NEWID", {
  sql <- translateSql("SELECT NEWID()", sourceDialect = "sql server", targetDialect = "redshift")$sql
  expect_equal(sql, "SELECT MD5(RANDOM()::TEXT || GETDATE()::TEXT)")
})

test_that("translateSQL sql server -> RedShift BOOL TYPE", {
  sql <- translateSql("CREATE TABLE table ( col BIT not null)", sourceDialect = "sql server", targetDialect = "redshift")$sql
  expect_equal(sql, "CREATE TABLE table ( col BOOLEAN not null)")
})

test_that("translateSQL sql server -> RedShift MONEY TYPE", {
  sql <- translateSql("CREATE TABLE table ( col MONEY not null)", sourceDialect = "sql server", targetDialect = "redshift")$sql
  expect_equal(sql, "CREATE TABLE table ( col DECIMAL(19, 4) not null)")
})

test_that("translateSQL sql server -> RedShift SMALLMONEY TYPE", {
  sql <- translateSql("CREATE TABLE table ( col SMALLMONEY not null)", sourceDialect = "sql server", targetDialect = "redshift")$sql
  expect_equal(sql, "CREATE TABLE table ( col DECIMAL(10, 4) not null)")
})

test_that("translateSQL sql server -> RedShift TINYINT TYPE", {
  sql <- translateSql("CREATE TABLE table ( col TINYINT not null)", sourceDialect = "sql server", targetDialect = "redshift")$sql
  expect_equal(sql, "CREATE TABLE table ( col SMALLINT not null)")
})

test_that("translateSQL sql server -> RedShift FLOAT TYPE", {
  sql <- translateSql("CREATE TABLE table ( col FLOAT(@s) not null)", sourceDialect = "sql server", targetDialect = "redshift")$sql
  expect_equal(sql, "CREATE TABLE table ( col FLOAT not null)")
})

test_that("translateSQL sql server -> RedShift DATETIME2 TYPE with precision specified", {
  sql <- translateSql("CREATE TABLE table ( col DATETIME2(@p) not null)", sourceDialect = "sql server", targetDialect = "redshift")$sql
  expect_equal(sql, "CREATE TABLE table ( col TIMESTAMP not null)")
})

test_that("translateSQL sql server -> RedShift DATETIME2 TYPE", {
  sql <- translateSql("CREATE TABLE table ( col DATETIME2 not null)", sourceDialect = "sql server", targetDialect = "redshift")$sql
  expect_equal(sql, "CREATE TABLE table ( col TIMESTAMP not null)")
})

test_that("translateSQL sql server -> RedShift DATETIME TYPE", {
  sql <- translateSql("CREATE TABLE table ( col DATETIME not null)", sourceDialect = "sql server", targetDialect = "redshift")$sql
  expect_equal(sql, "CREATE TABLE table ( col TIMESTAMP not null)")
})

test_that("translateSQL sql server -> RedShift SMALLDATETIME TYPE", {
  sql <- translateSql("CREATE TABLE table ( col SMALLDATETIME not null)", sourceDialect = "sql server", targetDialect = "redshift")$sql
  expect_equal(sql, "CREATE TABLE table ( col TIMESTAMP not null)")
})

test_that("translateSQL sql server -> RedShift DATETIMEOFFSET TYPE with precision specified", {
  sql <- translateSql("CREATE TABLE table ( col DATETIMEOFFSET(@p) not null)", sourceDialect = "sql server", targetDialect = "redshift")$sql
  expect_equal(sql, "CREATE TABLE table ( col TIMESTAMPTZ not null)")
})

test_that("translateSQL sql server -> RedShift DATETIMEOFFSET TYPE", {
  sql <- translateSql("CREATE TABLE table ( col DATETIMEOFFSET not null)", sourceDialect = "sql server", targetDialect = "redshift")$sql
  expect_equal(sql, "CREATE TABLE table ( col TIMESTAMPTZ not null)")
})

test_that("translateSQL sql server -> RedShift TEXT TYPE", {
  sql <- translateSql("CREATE TABLE table ( col TEXT not null)", sourceDialect = "sql server", targetDialect = "redshift")$sql
  expect_equal(sql, "CREATE TABLE table ( col VARCHAR(max) not null)")
})

test_that("translateSQL sql server -> RedShift NTEXT TYPE", {
  sql <- translateSql("CREATE TABLE table ( col NTEXT not null)", sourceDialect = "sql server", targetDialect = "redshift")$sql
  expect_equal(sql, "CREATE TABLE table ( col VARCHAR(max) not null)")
})

test_that("translateSQL sql server -> RedShift UNIQUEIDENTIFIER TYPE", {
  sql <- translateSql("CREATE TABLE table ( col UNIQUEIDENTIFIER not null)", sourceDialect = "sql server", targetDialect = "redshift")$sql
  expect_equal(sql, "CREATE TABLE table ( col CHAR(36) not null)")
})

test_that("translateSQL sql server -> RedShift STDEV POP", {
  sql <- translateSql("SELECT STDEVP(col) FROM table", sourceDialect = "sql server", targetDialect = "redshift")$sql
  expect_equal(sql, "SELECT STDDEV_POP(col) FROM table")
})

test_that("translateSQL sql server -> RedShift VAR POP", {
  sql <- translateSql("SELECT VARP(col) FROM table", sourceDialect = "sql server", targetDialect = "redshift")$sql
  expect_equal(sql, "SELECT VAR_POP(col) FROM table")
})

test_that("translateSQL sql server -> RedShift DATEDIFF_BIG", {
  sql <- translateSql("SELECT DATEDIFF_BIG(dd, start, end) FROM table", sourceDialect = "sql server", targetDialect = "redshift")$sql
  expect_equal(sql, "SELECT DATEDIFF(day, start, end) FROM table")
})

test_that("translateSQL sql server -> RedShift DATE PART yyyy", {
  sql <- translateSql("SELECT DATEPART(yyyy, start) FROM table", sourceDialect = "sql server", targetDialect = "redshift")$sql
  expect_equal(sql, "SELECT DATE_PART(year, start) FROM table")
})

test_that("translateSQL sql server -> RedShift DATE PART yy", {
  sql <- translateSql("SELECT DATEPART(yy, start) FROM table", sourceDialect = "sql server", targetDialect = "redshift")$sql
  expect_equal(sql, "SELECT DATE_PART(year, start) FROM table")
})

test_that("translateSQL sql server -> RedShift DATE PART mm", {
  sql <- translateSql("SELECT DATEPART(mm, start) FROM table", sourceDialect = "sql server", targetDialect = "redshift")$sql
  expect_equal(sql, "SELECT DATE_PART(month, start) FROM table")
})

test_that("translateSQL sql server -> RedShift DATE PART m", {
  sql <- translateSql("SELECT DATEPART(m, start) FROM table", sourceDialect = "sql server", targetDialect = "redshift")$sql
  expect_equal(sql, "SELECT DATE_PART(month, start) FROM table")
})

test_that("translateSQL sql server -> RedShift DATE PART dd", {
  sql <- translateSql("SELECT DATEPART(dd, start) FROM table", sourceDialect = "sql server", targetDialect = "redshift")$sql
  expect_equal(sql, "SELECT DATE_PART(day, start) FROM table")
})

test_that("translateSQL sql server -> RedShift DATE PART other", {
  sql <- translateSql("SELECT DATEPART(year, start), DATEPART(month, start), DATEPART(day, start) FROM table", sourceDialect = "sql server", targetDialect = "redshift")$sql
  expect_equal(sql, "SELECT DATE_PART(year, start), DATE_PART(month, start), DATE_PART(day, start) FROM table")
})

test_that("translateSQL sql server -> RedShift DATETIME2FROMPARTS", {
  sql <- translateSql("SELECT DATETIME2FROMPARTS(year,month,day,hour,minute,seconds, 0, 0) FROM table", sourceDialect = "sql server", targetDialect = "redshift")$sql
  expect_equal(sql, "SELECT CAST(TO_CHAR(year,'0000FM')||'-'||TO_CHAR(month,'00FM')||'-'||TO_CHAR(day,'00FM')||' '||TO_CHAR(hour,'00FM')||':'||TO_CHAR(minute,'00FM')||':'||TO_CHAR(seconds,'00FM') as TIMESTAMP) FROM table")
})

test_that("translateSQL sql server -> RedShift DATETIME2FROMPARTS with fractions", {
  sql <- translateSql("SELECT DATETIME2FROMPARTS(year,month,day,hour,minute,seconds,fractions,precision) FROM table", sourceDialect = "sql server", targetDialect = "redshift")$sql
  expect_equal(sql, "SELECT CAST(TO_CHAR(year,'0000FM')||'-'||TO_CHAR(month,'00FM')||'-'||TO_CHAR(day,'00FM')||' '||TO_CHAR(hour,'00FM')||':'||TO_CHAR(minute,'00FM')||':'||TO_CHAR(seconds,'00FM')||'.'||TO_CHAR(fractions,repeat('0', precision) || 'FM') as TIMESTAMP) FROM table")
})

test_that("translateSQL sql server -> RedShift DATETIMEOFFSETFROMPARTS", {
  sql <- translateSql("SELECT DATETIMEOFFSETFROMPARTS(year,month,day,hour,minute,seconds, 0,h_offset,m_offset, 0) FROM table", sourceDialect = "sql server", targetDialect = "redshift")$sql
  expect_equal(sql, "SELECT CAST(TO_CHAR(year,'0000FM')||'-'||TO_CHAR(month,'00FM')||'-'||TO_CHAR(day,'00FM')||' '||TO_CHAR(hour,'00FM')||':'||TO_CHAR(minute,'00FM')||':'||TO_CHAR(seconds,'00FM')||case when h_offset >= 0 then '+' else '-' end ||TO_CHAR(ABS(h_offset),'00FM')||':'||TO_CHAR(ABS(m_offset),'00FM') as TIMESTAMPTZ) FROM table")
})

test_that("translateSQL sql server -> RedShift DATETIMEOFFSETFROMPARTS with fractions", {
  sql <- translateSql("SELECT DATETIMEOFFSETFROMPARTS(year,month,day,hour,minute,seconds,fractions,h_offset,m_offset,precision) FROM table", sourceDialect = "sql server", targetDialect = "redshift")$sql
  expect_equal(sql, "SELECT CAST(TO_CHAR(year,'0000FM')||'-'||TO_CHAR(month,'00FM')||'-'||TO_CHAR(day,'00FM')||' '||TO_CHAR(hour,'00FM')||':'||TO_CHAR(minute,'00FM')||':'||TO_CHAR(seconds,'00FM')||'.'||TO_CHAR(fractions,repeat('0',precision) || 'FM')||case when h_offset >= 0 then '+' else '-' end ||TO_CHAR(ABS(h_offset),'00FM')||':'||TO_CHAR(ABS(m_offset),'00FM') as TIMESTAMPTZ) FROM table")
})

test_that("translateSQL sql server -> RedShift GETUTCDATE", {
  sql <- translateSql("SELECT GETUTCDATE();", sourceDialect = "sql server", targetDialect = "redshift")$sql
  expect_equal(sql, "SELECT CURRENT_TIMESTAMP;")
})

test_that("translateSQL sql server -> RedShift SMALLDATETIMEFROMPARTS", {
  sql <- translateSql("SELECT SMALLDATETIMEFROMPARTS(year,month,day,hour,minute) FROM table", sourceDialect = "sql server", targetDialect = "redshift")$sql
  expect_equal(sql, "SELECT CAST(TO_CHAR(year,'0000FM')||'-'||TO_CHAR(month,'00FM')||'-'||TO_CHAR(day,'00FM')||' '||TO_CHAR(hour,'00FM')||':'||TO_CHAR(minute,'00FM') as TIMESTAMP) FROM table")
})

test_that("translateSQL sql server -> RedShift SYSUTCDATETIME", {
  sql <- translateSql("SELECT SYSUTCDATETIME();", sourceDialect = "sql server", targetDialect = "redshift")$sql
  expect_equal(sql, "SELECT CURRENT_TIMESTAMP;")
})

test_that("translateSQL sql server -> RedShift ATN2", {
  sql <- translateSql("SELECT ATN2(a, b) FROM table", sourceDialect = "sql server", targetDialect = "redshift")$sql
  expect_equal(sql, "SELECT ATAN2(a, b) FROM table")
})

test_that("translateSQL sql server -> RedShift TRUNCATION OF NUMBER", {
  sql <- translateSql("SELECT ROUND(expression,length,trunc) FROM table", sourceDialect = "sql server", targetDialect = "redshift")$sql
  expect_equal(sql, "SELECT case when trunc = 0 then ROUND(CAST(expression AS FLOAT),length) else TRUNC(expression,length) end FROM table")
})

test_that("translateSQL sql server -> RedShift CHARINDEX from position", {
  sql <- translateSql("SELECT CHARINDEX('test',column, 3) FROM table", sourceDialect = "sql server", targetDialect = "redshift")$sql
  expect_equal(sql, "SELECT case when CHARINDEX('test', SUBSTRING(column, 3)) > 0 then (CHARINDEX('test', SUBSTRING(column, 3)) + 3 - 1) else 0 end FROM table")
})

test_that("translateSQL sql server -> RedShift QUOTENAME", {
  sql <- translateSql("SELECT QUOTENAME(a) FROM table", sourceDialect = "sql server", targetDialect = "redshift")$sql
  expect_equal(sql, "SELECT QUOTE_IDENT(a) FROM table")
})

test_that("translateSQL sql server -> RedShift SPACE", {
  sql <- translateSql("SELECT SPACE(n) FROM table", sourceDialect = "sql server", targetDialect = "redshift")$sql
  expect_equal(sql, "SELECT REPEAT(' ',n) FROM table")
})

test_that("translateSQL sql server -> RedShift STUFF", {
  sql <- translateSql("SELECT STUFF(expression, start, length, replace) FROM table", sourceDialect = "sql server", targetDialect = "redshift")$sql
  expect_equal(sql, "SELECT SUBSTRING(expression, 0, start)||replace||SUBSTRING(expression, start + length) FROM table")
})

test_that("translateSQL sql server -> RedShift CONCAT", {
  sql <- translateSql(
    "SELECT CONCAT(p1,p2,p3,p4,p5,p6,p7) FROM table", 
    sourceDialect = "sql server", targetDialect = "redshift")$sql
  expect_equal(sql, "SELECT CONCAT(p1,CONCAT(p2,CONCAT(p3,CONCAT(p4,CONCAT(p5,CONCAT(p6,p7)))))) FROM table")
})

test_that("translateSQL sql server -> RedShift CTAS TEMP WITH CTE person_id", {
  sql <- translateSql(
    "WITH a AS b SELECT person_id, col1, col2 INTO #table FROM person;", 
    sourceDialect = "sql server", targetDialect = "redshift")$sql
  expect_equal(sql, 
  "CREATE TABLE #table\nDISTKEY(person_id)\nAS\nWITH\na\nAS\nb\nSELECT\n person_id , col1, col2\nFROM\nperson;")
})

test_that("translateSQL sql server -> RedShift CTA WITH CTE person_id", {
  sql <- translateSql(
    "WITH a AS b SELECT person_id, col1, col2 INTO table FROM person;", 
    sourceDialect = "sql server", targetDialect = "redshift")$sql
  expect_equal(sql, 
  "CREATE TABLE table\nDISTKEY(person_id)\nAS\nWITH\na\nAS\nb\nSELECT\n person_id , col1, col2\nFROM\nperson;")
})

test_that("translateSQL sql server -> RedShift CTAS TEMP person_id", {
  sql <- translateSql(
    "SELECT person_id, col1, col2 INTO #table FROM person;", 
    sourceDialect = "sql server", targetDialect = "redshift")$sql
  expect_equal(sql, 
  "CREATE TABLE #table\nDISTKEY(person_id)\nAS\nSELECT\n person_id , col1, col2\nFROM\nperson;")
})

test_that("translateSQL sql server -> RedShift CTAS person_id", {
  sql <- translateSql(
    "SELECT person_id, col1, col2 INTO table FROM person;", 
    sourceDialect = "sql server", targetDialect = "redshift")$sql
  expect_equal(sql, 
  "CREATE TABLE table\nDISTKEY(person_id)\nAS\nSELECT\n person_id , col1, col2\nFROM\nperson;")
})

test_that("translateSQL sql server -> RedShift CREATE TABLE person_id", {
  sql <- translateSql(
    "CREATE TABLE [dbo].[drug_era] ([drug_era_id] bigint NOT NULL, [person_id] bigint NOT NULL, [drug_concept_id] bigint NOT NULL, [drug_era_start_date] date NOT NULL, [drug_era_end_date] date NOT NULL, [drug_exposure_count] int NULL, [gap_days] int NULL);", 
    sourceDialect = "sql server", targetDialect = "redshift")$sql
  expect_equal(sql, 
  "CREATE TABLE [dbo].[drug_era] ([drug_era_id] bigint NOT NULL, [person_id] bigint NOT NULL, [drug_concept_id] bigint NOT NULL, [drug_era_start_date] date NOT NULL, [drug_era_end_date] date NOT NULL, [drug_exposure_count] int NULL, [gap_days] int NULL)\nDISTKEY(person_id);")
})

test_that("translateSQL sql server -> PDW CREATE TABLE person_id", {
  sql <- translateSql(
    "CREATE TABLE [dbo].[drug_era] ([drug_era_id] bigint NOT NULL, [person_id] bigint NOT NULL, [drug_concept_id] bigint NOT NULL, [drug_era_start_date] date NOT NULL, [drug_era_end_date] date NOT NULL, [drug_exposure_count] int NULL, [gap_days] int NULL);", 
    sourceDialect = "sql server", targetDialect = "pdw")$sql
  expect_equal(sql, 
  "IF XACT_STATE() = 1 COMMIT; CREATE TABLE  [dbo].[drug_era] ([drug_era_id] bigint NOT NULL, [person_id] bigint NOT NULL, [drug_concept_id] bigint NOT NULL, [drug_era_start_date] date NOT NULL, [drug_era_end_date] date NOT NULL, [drug_exposure_count] int NULL, [gap_days] int NULL)\nWITH (DISTRIBUTION = HASH(person_id));")
})

test_that("translateSQL sql server -> RedShift ISDATE", {
  sql <- translateSql(
    "SELECT * FROM table WHERE ISDATE(col) = 1", 
    sourceDialect = "sql server", targetDialect = "redshift")$sql
  expect_equal(sql, 
    "SELECT * FROM table WHERE REGEXP_INSTR(col, '^(\\\\d{4}[/\\-]?[01]\\\\d[/\\-]?[0123]\\\\d)([ T]([0-1][0-9]|[2][0-3]):([0-5][0-9])(:[0-5][0-9](.\\\\d+)?)?)?$') = 1")
})

test_that("translateSQL sql server -> RedShift ISNUMERIC", {
  sql <- translateSql(
    "SELECT * FROM table WHERE ISNUMERIC(col) = 1", 
    sourceDialect = "sql server", targetDialect = "redshift")$sql
  expect_equal(sql, 
    "SELECT * FROM table WHERE REGEXP_INSTR(col, '^[\\-\\+]?(\\\\d*\\\\.)?\\\\d+([Ee][\\-\\+]?\\\\d+)?$') = 1")
})

test_that("translateSQL sql server -> RedShift PATINDEX", {
  sql <- translateSql(
    "SELECT PATINDEX(pattern,expression) FROM table;",
    sourceDialect = "sql server", targetDialect = "redshift")$sql
  expect_equal(sql, 
    "SELECT REGEXP_INSTR(expression, case when LEFT(pattern,1)<>'%' and RIGHT(pattern,1)='%' then '^' else '' end||TRIM('%' FROM REPLACE(pattern,'_','.'))||case when LEFT(pattern,1)='%' and RIGHT(pattern,1)<>'%' then '$' else '' end) FROM table;")
})

test_that("translateSQL sql server -> RedShift CREATE TABLE IF NOT EXISTS with hashing", {
  sql <- translateSql(
    "IF OBJECT_ID('cdm.heracles_results', 'U') IS NULL CREATE TABLE cdm.heracles_results (cohort_definition_id int, analysis_id int, stratum_1 varchar(255), stratum_2 varchar(255), stratum_3 varchar(255), stratum_4 varchar(255), stratum_5 varchar(255), count_value bigint, last_update_time datetime) DISTKEY(analysis_id);",
    sourceDialect = "sql server", targetDialect = "redshift")$sql
  expect_equal(sql, 
    "CREATE TABLE IF NOT EXISTS cdm.heracles_results (cohort_definition_id int, analysis_id int, stratum_1 varchar(255), stratum_2 varchar(255), stratum_3 varchar(255), stratum_4 varchar(255), stratum_5 varchar(255), count_value bigint, last_update_time TIMESTAMP) DISTKEY(analysis_id);")
})
