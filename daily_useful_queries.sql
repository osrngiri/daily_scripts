-- Gather execution plan
SELECT * FROM TABLE(DBMS_XPLAN.display_cursor(sql_id=>'sql_id',format=>'ALLSTATS LAST'));

select * from table(dbms_xplan.display_awr('&sqlid',NULL,NULL,'ALLSTATS +PEEKED_BINDS -NOTE +PARTITION +IOSTATS +COST +BYTES -PROJECTION -ALIAS +PREDICATE'));

-- Generate AWR report
SELECT * FROM TABLE(sys.DBMS_WORKLOAD_REPOSITORY.awr_report_html(<DB Id>,<Inst No>,<Start_snap_id><End_snap_id>)); --Query to get AWR report
<DB Id>,<Inst No>,<Start_snap_id><End_snap_id>
--================================================================================================
-- Determine query details
SELECT
    sql_id,
    fetches,
    executions,
    first_load_time,
    last_load_time,
    disk_reads,
    buffer_gets,
    application_wait_time,
    rows_processed,
    cpu_time "CPU TIME(in micro seconds)",
    elapsed_time  "ELA TIME(in micro seconds)",
    sql_plan_baseline
FROM
    gv$sqlarea
WHERE
    sql_id = '&slq_id'

-- Query to find time remaining to complete long running execution step of SQL
SELECT
    opname      target,
    round((sofar / totalwork), 4) * 100 percentage_complete,
    start_time,
    ceil(time_remaining / 60) max_time_remaining_in_min,
    floor(elapsed_seconds / 60) time_spent_in_min,
    ar.sql_fulltext,
    ar.parsing_schema_name,
    ar.module   client_tool
FROM
    v$session_longops l,
    v$sqlarea ar
WHERE ar.sql_id='d8cg5rbf3a5c0'
    and l.sql_id = ar.sql_id
    AND totalwork > 0
    AND ar.users_executing > 0
    AND sofar != totalwork;

-- Generate SQL execution plan
SELECT
      LPAD(' ',depth)||
      OPERATION||'_'||
      OPTIONS||' '||
      OBJECT_NAME  operation
FROM
       DBA_HIST_SQL_PLAN
WHERE
        sql_id='&SQL_ID'
order by  ID,PLAN_HASH_VALUE;

--The following script will identify those SQL statement that have more than one execution plan in the library cache.  
--Note that this SQL also looks into historical SQL plans (using dba_hist_sqlstat).  
--This is an important report for showing the effect of DDL and statistics changes on the execution plans of SQL 
--in a controlled, production environment.

select
   vs.sid,
   vs.sql_id,
   vs.last_call_et,
   sq.plan_hash_value
from
   v$session vs,
   v$sql     sq
where
   vs.sql_id=sq.sql_id
and
   vs.sql_child_number=sq.child_number
and
   sq.plan_hash_value not in
   (select
      ss.plan_hash_value
   from
      dba_hist_sqlstat ss
   where
      ss.sql_id=sq.sql_id) and
   0 <
      (select
           count(ss.plan_hash_value)
       from
           dba_hist_sqlstat ss
   where
      ss.sql_id=sq.sql_id);

========================================


select
    sid,
    sql_text
from
    v$session s,
    v$sql q
where
    sid in
    (select
       sid
    from
       v$session
   where
       state in ('WAITING')
   and
       wait_class != 'Idle'
   and
       event='enq: TX - row lock contention'
   and
      (q.sql_id = s.sql_id or q.sql_id = s.prev_sql_id));

-- Get hsitorically execution time of a SQL
SELECT
    a.instance_number   inst_id,
    a.snap_id,
    a.plan_hash_value,
    TO_CHAR(begin_interval_time, 'dd-mon-yy hh24:mi') btime,
    abs(EXTRACT(MINUTE FROM(end_interval_time - begin_interval_time)) + EXTRACT(HOUR FROM(end_interval_time - begin_interval_time
    )) * 60 + EXTRACT(DAY FROM(end_interval_time - begin_interval_time)) * 24 * 60) minutes,
    executions_delta    executions,
    round(elapsed_time_delta / 1000000 / greatest(executions_delta, 1), 4) "avg duration (sec)"
FROM
    dba_hist_sqlstat a,
    dba_hist_snapshot b
WHERE
    sql_id = '<SQL_ID>'
    AND a.snap_id = b.snap_id
    AND a.instance_number = b.instance_number
ORDER BY
    snap_id DESC,
    a.instance_number;


--Check available plan baselines

SELECT PLAN_TABLE_OUTPUT
FROM   V$SQL s, DBA_SQL_PLAN_BASELINES b, 
       TABLE(
       DBMS_XPLAN.DISPLAY_SQL_PLAN_BASELINE(b.sql_handle,b.plan_name,'basic') 
       ) t
WHERE  s.EXACT_MATCHING_SIGNATURE=b.SIGNATURE
AND    b.PLAN_NAME=s.SQL_PLAN_BASELINE
AND    s.SQL_ID='6nd5yvp4ksgxt';
--
SELECT SID, EVENT, P1, P2 , P3 FROM v$session_wait where event='db file sequential read'

SELECT
    owner,
    segment_name,
    segment_type
FROM
    dba_extents
WHERE
    file_id = P1
    AND P2 BETWEEN block_id AND block_id + blocks - 1

--Table HWM
select
    a.owner,
    a.table_name,
    b.blocks                        alcblks,
    a.blocks                        usdblks,
    (b.blocks-a.empty_blocks-1)     hgwtr
from
    dba_tables a,
    dba_segments b
where
    a.table_name=b.segment_name
    and a.owner=b.owner
    and a.owner not in('SYS','SYSTEM')
    and a.blocks <> (b.blocks-a.empty_blocks-1)
    and a.owner like upper('&owner')||'%'
    and a.table_name like upper('&table_name')||'%'
order by 1,2;

SELECT file_name, hwm, blocks total_blocks, blocks-hwm+1 as Shrinkage_Available
FROM dba_data_files a,
     ( select file_id, max(block_id+blocks) hwm
       from dba_extents where segment_name='<SEGMENT_NAME>' and segment_type='<SEGMENT_TYPE>'
       group by file_id ) b
WHERE a.file_id = b.file_id
-----
-- PGA memory usage by session
SELECT NVL(s.username, '(oracle)') AS username,
       s.osuser,
       s.sid,
       s.serial#,
       p.spid,
       ROUND(p.pga_used_mem/1024/1024,2) AS pga_used_mem_mb,
       ROUND(p.pga_alloc_mem/1024/1024,2) AS pga_alloc_mem_mb,
       ROUND(p.pga_freeable_mem/1024/1024,2) AS pga_freeable_mem_mb,
       ROUND(p.pga_max_mem/1024/1024,2) AS pga_max_mem_mb,
       s.lockwait,
       s.status,
       s.service_name,
       s.module,
       s.machine,
       s.program,
       TO_CHAR(s.logon_Time,'DD-MON-YYYY HH24:MI:SS') AS logon_time,
       s.last_call_et AS last_call_et_secs
FROM   v$session s,
       v$process p
WHERE  s.paddr = p.addr
ORDER BY s.username, s.osuser;

----

ANALYZE INDEX  emp_id_idx  VALIDATE STRUCTURE;
SELECT
    name
  , del_lf_rows
  , lf_rows - del_lf_rows lf_rows_used
  , TO_CHAR( del_lf_rows /(DECODE(lf_rows,0,0.01,lf_rows))*100,'999.99999') ibadness 
FROM   index_stats;

--Open cursors queries 

1) select a.value, s.username, s.sid, s.serial# 
from gv$sesstat a, gv$statname b, gv$session s 
where a.statistic# = b.statistic#  
and s.sid=a.sid 
and a.inst_id = b.inst_id
and b.name = 'opened cursors current' 
and s.username is not null;

2) select  sid ,sql_text, count(*) as "OPEN CURSORS", USER_NAME 
from gv$open_cursor group by sid, sql_text;

3) SELECT  max(a.value) as highest_open_cur
, p.value as max_open_cur 
FROM gv$sesstat a, gv$statname b, gv$parameter p 
WHERE  a.statistic# = b.statistic#  
and a.inst_id = b.inst_id
and b.name = 'opened cursors current' 
and p.name= 'open_cursors' 
group by p.value;

-- Object causing db file sequential read

select segment_name, partition_name, segment_type, tablespace_name
from   dba_extents a, v$session_wait b
where  b.p2 between a.block_id and (a.block_id + a.blocks - 1)
and    a.file_id  = b.p1
and    b.event    = 'db file sequential read';

--The SQL statement associated with this event can be obtained using this query:

select a.sid, a.serial#, a.username, a.osuser, b.sql_text
from   v$session a, v$sqltext b
where  a.sql_hash_value = b.hash_value
and    a.sql_address    = b.address
and    a.sid in (select sid
                 from   v$session_wait
                 where  event = 'db file sequential read')
order by a.sid, b.hash_value, b.piece;

-- Description : Shows objects responsible for db file sequential read wait event

select b.sid,
nvl(substr(a.object_name,1,30),
'P1='||b.p1||' P2='||b.p2||' P3='||b.p3) object_name,
a.subobject_name,
a.object_type
from dba_objects a, v$session_Wait b, x$bh c
where c.obj = a.object_id(+)
and b.p1 = c.file#(+)
and b.p2 = c.dbablk(+)
and b.event = 'db file sequential read'
union
select b.sid,
nvl(substr(a.object_name,1,30),'P1='||b.p1||' P2='||b.p2||' P3='||b.p3) object_name,
a.subobject_name,
a.object_type
from dba_objects a, v$session_Wait b, x$bh c
where c.obj = a.data_object_id(+)
and b.p1 = c.file#(+)
and b.p2 = c.dbablk(+)
and b.event = 'db file sequential read'
order by 1;

-- Shows locks for a DML
SELECT TYPE,name,
       lock_mode(lmode) lock_mode , id1, id2, lmode,
       DECODE(TYPE, 'TM',(SELECT object_name
                            FROM dba_objects
                           WHERE object_id = id1))
            table_name
 FROM v$lock JOIN v$lock_type USING (type)
WHERE sid = (SELECT sid
             FROM v$session
             WHERE audsid = USERENV('sessionid'))
  and type <> 'AE';

--Description : Lock tree
WITH sessions AS
   (SELECT /*+materialize*/
           sid, blocking_session, row_wait_obj#, sql_id
      FROM v$session)
SELECT LPAD(' ', LEVEL ) || sid sid, object_name, 
       substr(sql_text,1,40) sql_text
  FROM sessions s 
  LEFT OUTER JOIN dba_objects 
       ON (object_id = row_wait_obj#)
  LEFT OUTER JOIN v$sql
       USING (sql_id)
 WHERE sid IN (SELECT blocking_session FROM sessions)
    OR blocking_session IS NOT NULL
 CONNECT BY PRIOR sid = blocking_session
 START WITH blocking_session IS NULL; 

--Description : Shows blocking and blocker sessions
select
(select username from v$session where sid=a.sid) blocker,
a.sid,
' is blocking ',
(select username from v$session where sid=b.sid) blockee,
b.sid
from v$lock a, v$lock b
where a.block = 1
and b.request > 0
and a.id1 = b.id1
and a.id2 = b.id2;

--Description: Show SQLs with the highest lock waits
WITH sql_app_waits AS 
    (SELECT sql_id, SUBSTR(sql_text, 1, 80) sql_text, 
            application_wait_time/1000 app_time_ms,
            elapsed_time,
            ROUND(application_wait_time * 100 / 
                elapsed_time, 2) app_time_pct,
            ROUND(application_wait_time * 100 / 
                SUM(application_wait_time) OVER (), 2) pct_of_app_time,
            RANK() OVER (ORDER BY application_wait_Time DESC) ranking
       FROM v$sql
      WHERE elapsed_time > 0
        AND application_wait_time>0 )
SELECT sql_text, app_time_ms, app_time_pct,
       pct_of_app_time, ranking
FROM sql_app_waits
WHERE ranking <= 10
ORDER BY ranking  ; 

--Description: Show sessions with a specific USERNAME and their lock waits
WITH session_event AS 
  (SELECT CASE WHEN event LIKE 'enq:%' 
              THEN event  ELSE wait_class
          END wait_type, e.*
     FROM v$session_event e   )
SELECT  wait_type,SUM(total_waits) total_waits,
       round(SUM(time_waited_micro)/1000000,2) time_waited_seconds,
       ROUND(  SUM(time_waited_micro)
             * 100
             / SUM(SUM(time_waited_micro)) OVER (), 2) pct
FROM (SELECT  e.sid, wait_type, event, total_waits, time_waited_micro
      FROM    session_event e
      UNION
      SELECT  sid, 'CPU', stat_name, NULL, VALUE
      FROM v$sess_time_model
      WHERE stat_name IN ('background cpu time', 'DB CPU')) l
WHERE wait_type <> 'Idle'
 and sid in (select sid from v$session where username='OPSG') 
GROUP BY wait_type 
ORDER BY 4 DESC

-- query to find name & location of trace file 
select c.value || '/' || instance || '_ora_' ||
       ltrim(to_char(a.spid,'fm99999')) || '.trc'
  from v$process a, v$session b, v$parameter c, v$thread c
 where a.addr = b.paddr
   and b.audsid = userenv('sessionid')
   and c.name = 'user_dump_dest';
   
-- Explain plan query
SELECT     ID, parent_id, POSITION, object_instance,
              RPAD (' ', 2 * LEVEL)
           || operation
           || ' '
           || DECODE (optimizer, NULL, NULL, '(' || LOWER (optimizer) || ') ')
           || object_type
           || ' '
           || object_owner
           || ' '
           || object_name
           || ' '
           || DECODE (options, NULL, NULL, '(' || LOWER (options) || ') ')
           || other_tag
           || ' '
           || DECODE (partition_id,
                      NULL, NULL,
                      'Pt id: ' || partition_id || ' '
                     )
           || DECODE (partition_start,
                      NULL, NULL,
                         'Pt Range: '
                      || partition_start
                      || ' - '
                      || partition_stop
                      || ' '
                     )
           || DECODE (distribution,
                      NULL, NULL,
                      'Distribution: ' || distribution || ' '
                     )
           || DECODE (COST,
                      NULL, NULL,
                      'Cost (' || COST || ',' || CARDINALITY || ',' || BYTES
                      || ') '
                     )
           || DECODE (search_columns,
                      NULL, NULL,
                      '(Columns ' || search_columns || ') '
                     ) PLAN
      FROM plan_table
START WITH ID = 0 AND STATEMENT_ID = '&m_statement_id'
CONNECT BY (parent_id = PRIOR ID AND STATEMENT_ID = PRIOR STATEMENT_ID)
        OR (    ID = 0
            AND PRIOR NVL (object_name, ' ') LIKE 'SYS_LE%'
            AND NVL (STATEMENT_ID, ' ') = PRIOR NVL (object_name, ' ')
           )
  ORDER BY ID;
  
  -- Decode P1 value to find type of lock 
  
  create or replace
  function enqueue_decode( l_p1 in number ) return varchar2
  as
  l_str varchar2(25);
  begin
  select chr(bitand(l_p1,-16777216)/16777215)||
  chr(bitand(l_p1, 16711680)/65535) || ' ' ||
  decode( bitand(l_p1, 65535),
   0, 'No lock',
   1, 'No lock',
   2, 'Row-Share',
   3, 'Row-Exclusive',
   4, 'Share',
   5, 'Share Row-Excl',
   6, 'Exclusive' )
   into l_str
   from dual;
  
 return l_str;
 end  enqueue_decode;
 
================================================================
 
 create or replace type myTableType as table of varchar2(50);
 
 create or replace function str2tbl( p_str in varchar2 ) return myTableType
  as
      l_str   long default p_str || ',';
      l_n        number;
      l_data    myTableType := myTabletype();
  begin
      loop
          l_n := instr( l_str, ',' );
          exit when (nvl(l_n,0) = 0);
          l_data.extend;
          l_data( l_data.count ) := ltrim(rtrim(substr(l_str,1,l_n-1)));
          l_str := substr( l_str, l_n+1 );
      end loop;
      return l_data;
  end;
  
  
 --Example 
  select * from all_users
  where user_id in ( select *
  from THE ( select cast( str2tbl( '1, 3, 5, 7, 99' ) as mytableType ) from dual ) )
  
  
  --Single query approach 
  SELECT TRIM (SUBSTR (txt,
                           INSTR (txt, ',', 1, LEVEL) + 1,
                             INSTR (txt, ',', 1, LEVEL + 1)
                           - INSTR (txt, ',', 1, LEVEL)
                           - 1
                          )
                  ) AS token
        FROM (SELECT ',' || :txt || ',' txt
                FROM DUAL)
CONNECT BY LEVEL <= LENGTH (:txt) - LENGTH (REPLACE (:txt, ',', '')) + 1

select * from emp where ename in (
select regexp_substr('SMITH,ALLEN,WARD,JONES','[^,]+', 1, level) from dual
connect by regexp_substr('SMITH,ALLEN,WARD,JONES', '[^,]+', 1, level) is not null );
   
----------------------------------------------------------------------   
--Sessions Blocking Other Sessions Report
SELECT
      a.session_id,
      username,
      type,
      mode_held,
      mode_requested,
      lock_id1,
      lock_id2
FROM
      sys.v_$session b,
      sys.dba_blockers c,
      sys.dba_lock a
WHERE
      c.holding_session=a.session_id and
      c.holding_session=b.sid
      
=============================================================================
--Report on sessions waiting for locks
SELECT
      holding_session bsession_id,
      waiting_session wsession_id,
      b.username busername,
      a.username wusername,
      c.lock_type type,
      mode_held, mode_requested,
      lock_id1, lock_id2
FROM
sys.v_$session b, sys.dba_waiters c, sys.v_$session a
WHERE
c.holding_session=b.sid and
c.waiting_session=a.sid
============================================================================
--A script that shows locks
select session_id "sid",SERIAL#  "Serial",
substr(object_name,1,20) "Object",
  substr(os_user_name,1,10) "Terminal",
  substr(oracle_username,1,10) "Locker",
  nvl(lockwait,'active') "Wait",
  decode(locked_mode,
    2, 'row share',
    3, 'row exclusive',
    4, 'share',
    5, 'share row exclusive',
    6, 'exclusive',  'unknown') "Lockmode",
  OBJECT_TYPE "Type"
FROM
  SYS.V_$LOCKED_OBJECT A,
  SYS.ALL_OBJECTS B,
  SYS.V_$SESSION c
WHERE
  A.OBJECT_ID = B.OBJECT_ID AND
  C.SID = A.SESSION_ID
ORDER BY 1 ASC, 5 Desc

 
================================

SELECT df.tablespace_name "Tablespace",
  totalusedspace "Used MB",
  (df.totalspace - tu.totalusedspace) "Free MB",
  df.totalspace "Total MB",
  ROUND(100 * ( (df.totalspace - tu.totalusedspace)/ df.totalspace)) "% Free"
FROM
  (SELECT tablespace_name,
    ROUND(SUM(bytes) / 1048576) TotalSpace
  FROM dba_data_files
  GROUP BY tablespace_name
  ) df,
  (SELECT ROUND(SUM(bytes)/(1024*1024)) totalusedspace,
    tablespace_name
  FROM dba_segments
  GROUP BY tablespace_name
  ) tu
WHERE df.tablespace_name = tu.tablespace_name


SELECT
    tablespace_name   "TEMP TBS NAME",
    bytes / ( 1024 * 1024 ) "SIZE(MBs)",
    bytes_used / ( 1024 * 1024 ) "BYTES USED(MBs)",
    bytes_free / ( 1024 * 1024 ) "BYTES FREE(MBs)"
FROM
    sys.v_$temp_space_header,
    v$tempfile;