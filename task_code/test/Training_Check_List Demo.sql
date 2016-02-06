/*========================General Sort Considerations================
1.Script format
2.Script Standardization
3.Commonly Errors
4.Suggestions
5.Special topics
*/

/*==========================First part:Script format=====================*/

-------------------------------------------------------------------
/*
1.T-SQL脚本有必要的缩进和换行，代码层次结构清晰，
一行的最大长度一般不要87个字符；代码使用统一的风格，
例如：如果使用空格作为缩进，则不能再使用TAB做缩进处理
*/
USE Test
GO
--错误的格式
DECLARE @TransactionNumber INT
		,@purno CHAR(8)
SET @purno='105336'
IF ISNULL(@purno,'')<>''
BEGIN
SELECT TOP 1 @TransactionNumber=TransactionNumber FROM [SCM].[dbo].[potran01] WITH (NOLOCK) WHERE purno=@purno
END

--正确的格式:
DECLARE @TransactionNumber INT
		,@purno CHAR(8)
SET @purno='105336'
IF ISNULL(@purno,'')<>''
BEGIN
	SELECT TOP 1 @TransactionNumber=TransactionNumber 
	FROM [SCM].[dbo].[potran01] WITH (NOLOCK)
	WHERE purno=@purno
END
-------------------------------------------------------------------
/*
2.脚本中的所有关键字、系统变量名、系统函数名全部大写
（可以参考一个SQL联机帮助中对于该关键字描述时使用的是大写还是小写）
*/

--错误写法
select case @@servicename
			when 'mssqlserver' then @@servername
			else @@servicename
		end as InstanceName

--正确写法
SELECT CASE @@SERVICENAME
			WHEN 'MSSQLSERVER' THEN @@SERVERNAME
			ELSE @@SERVICENAME
		END AS InstanceName
-------------------------------------------------------------------

/*创建、修改正式对象请添加必要的注释
存储过程、视图、用户定义函数有合理的注释，至少包括：创建人、
创建日期、修改人、修改日期、功能描述、参数说明。
*/

USE Test
GO

/*===========================Create SP==========================
**DB:Test
**Type:Procedure
**ObjectName:dbo.Up_Test_Print
**Creater:Cherish
**Create date:2008-11-7
**Modify by:Cherish
**Modify date:2008-11-8
**Function:Testing print in SSB
**Variable:N/A
=====================================================================*/
CREATE PROCEDURE dbo.Up_Test_Print
AS
SET NOCOUNT ON
BEGIN
	DECLARE @do INT
			,@loop INT
	SET @do=0
	SET @loop=100
	WHILE @do<@loop
		BEGIN
			PRINT 'Test Print in sp.'
			SET @do=@do+1
		END
END
GO
-------------------------------------------------------------------


/*==========================Second part:Script Standardization=====================*/

/*
--BYDBA 1.请在代码的开始处添加USE GO指令
*/
USE DB
GO
-------------------------------------------------------------------
/*
--BYDBA 1.请指定对象的架构者(SQL2K中称为所有者)（一般都是dbo）
*/
SELECT C1,C2 
FROM dbo.Test WITH (NOLOCK)
-------------------------------------------------------------------
/*
--BYDBA 1.已经在数据库中，不用加库名。
*/
USE Test
GO
CREATE VIEW dbo.V_TestView
AS
SELECT C1,C2
FROM [Test].[dbo].[Test] WITH (NOLOCK)--BYDBA 1.已经在数据库中，不用加库名。
-------------------------------------------------------------------

/*
--BYDBA 1.请显示罗列表字段。
*/
USE TEST
GO
IF OBJECT_ID('dbo.Test_SELECT') IS NOT NULL
	DROP TABLE dbo.Test_SELECT
CREATE TABLE dbo.Test_SELECT
(
	ID INT IDENTITY(1,1) NOT NULL
	,SONumber INT
	,CustomerNumber INT
	,ShippingCode CHAR(15)
	,CONSTRAINT PK_Test_SELECT PRIMARY KEY
	(
		ID ASC
	)
)

--不规范的写法
SELECT * 
FROM dbo.Test_SELECT WITH (NOLOCK)

--推荐的写法
SELECT ID
		, SONumber
		, CustomerNumber
		, ShippingCode 
FROM dbo.Test_SELECT WITH (NOLOCK)
-------------------------------------------------------------------
/*
--BYDBA 1.请指明字段的表别名。
*/

--不规范的写法
SELECT edate				--BYDBA 1.请指明字段的表别名。
		,ISNULL(vendno,'')	--BYDBA 1.请指明字段的表别名。
FROM [SCM].[dbo].[arinvt01] AS A WITH (NOLOCK)
	INNER JOIN [SCM].[dbo].[potran01] AS B WITH (NOLOCK)
	ON A.Item=B.Item
WHERE purno='105336'--BYDBA 1.请指明字段的表别名。

--推荐的写法
SELECT A.edate
		,ISNULL(B.vendno,'')
FROM [SCM].[dbo].[arinvt01] AS A WITH (NOLOCK)
	INNER JOIN [SCM].[dbo].[potran01] AS B WITH (NOLOCK)
	ON A.Item=B.Item
WHERE A.purno='105336'
-------------------------------------------------------------------
/*
6.在INSERT语句中，必须指定插入列的列表, 否则的话, 
表结构略有差异就会导致插入失败或者插入到不正确的列中
*/
USE TEST
GO
IF OBJECT_ID('dbo.Test_INSERT') IS NOT NULL
	DROP TABLE dbo.Test_INSERT
CREATE TABLE dbo.Test_INSERT
(
	ID INT  NOT NULL
	,SONumber INT
	,CustomerNumber INT
	,CONSTRAINT PK_Test_INSERT PRIMARY KEY
	(
		ID ASC
	)
)

IF OBJECT_ID('dbo.Test_INSERT1') IS NOT NULL
	DROP TABLE dbo.Test_INSERT1
CREATE TABLE dbo.Test_INSERT1
(
	ID INT NOT NULL
	,SONumber INT
	,CustomerNumber INT
	,CONSTRAINT PK_Test_INSERT1 PRIMARY KEY
	(
		ID ASC
	)
)

INSERT INTO dbo.Test_INSERT1(ID,SONumber,CustomerNumber)
SELECT 1,1,3

--错误的写法
INSERT INTO dbo.Test_INSERT--BYDBA 1.请显示指定列名称。
SELECT *
FROM dbo.Test_INSERT1 AS A WITH (NOLOCK)
WHERE NOT EXISTS(SELECT TOP 1 1
				FROM dbo.Test_INSERT AS B WITH (NOLOCK)
				WHERE A.ID=B.ID)
GO

--修改表
ALTER TABLE Test_INSERT
ADD ShippingCode CHAR(15)
GO

--再次插入数据报错。
INSERT INTO dbo.Test_INSERT
SELECT *
FROM dbo.Test_INSERT1 AS A WITH (NOLOCK)
WHERE NOT EXISTS(SELECT TOP 1 1
				FROM dbo.Test_INSERT AS B WITH (NOLOCK)
				WHERE A.ID=B.ID)
GO


--正确的写法为:
INSERT INTO dbo.Test_INSERT(ID
							, SONumber
							, CustomerNumber)
SELECT ID
		, SONumber
		, CustomerNumber
FROM dbo.Test_INSERT1 AS A WITH (NOLOCK)
WHERE NOT EXISTS(SELECT TOP 1 1
				FROM dbo.Test_INSERT AS B WITH (NOLOCK)
				WHERE A.ID=B.ID)
-------------------------------------------------------------------
/*
7.对于SELECT中涉及的表和视图，在非事务和特别的完整性要求的上下文中，
使用TABLE Hints—WITH(NOLOCK)
*/
--Demo:
USE TEST
GO
IF OBJECT_ID('dbo.Test_NOLOCK') IS NOT NULL
	DROP TABLE dbo.Test_NOLOCK
CREATE TABLE dbo.Test_NOLOCK
(
ID INT IDENTITY(1,1)
,NAME CHAR(36)
)

DECLARE @i INT
SET @i=0
BEGIN TRY
TRUNCATE TABLE dbo.Test_NOLOCK
	BEGIN TRAN
	WHILE @i<10
	BEGIN

		INSERT INTO Test_NOLOCK(NAME)
		SELECT NEWID()
		SET @i=@i+1
	END
--	COMMIT
END TRY
BEGIN CATCH
	ROLLBACK 
END CATCH

--不允许读脏，只能读取已经提交的数据
SELECT ID,NAME
FROM dbo.Test_NOLOCK

--可以读取没有提交的数据
SELECT ID,NAME
FROM dbo.Test_NOLOCK WITH (NOLOCK)

-------------------------------------------------------------------
--BYDBA 1.请使用WITH(NOLOCK)，而非NOLOCK。
--IN D2WHP01
select VendorNumber,
WareHouseNumber,
SoNumber,
ItemNumber,
PartNumber,
Price,
Quantity,
LargeFlag,
InsertDate,
InvoiceNumber 
from S7EDIDB01.EDI.DBO.EDIInvoiceMatchTransaction /*with*/(nolock)
where sonumber = '86022942'
-------------------------------------------------------------------
/*
8.子查询中，只查询出必须的列，不要包含与处理需求无关的列
*/
--错误的写法
SELECT ID
FROM dbo.Test_NOLOCK WITH (NOLOCK)
WHERE EXISTS (
				SELECT 1 AS ID
						, 'Name' AS Name
						, 'Test' AS C3
				UNION ALL
				SELECT 2
						, 'ABCE' AS Name
						, 'Test_NOLOCK' AS C3
			)

--正确的写法
SELECT ID
FROM dbo.Test_NOLOCK WITH (NOLOCK)
WHERE EXISTS( SELECT TOP 1 1 
				FROM
					(
						SELECT 1 AS ID
						UNION ALL
						SELECT 2
					) AS A
			)
-------------------------------------------------------------------
/*
--BYDBA 1.变量赋值,请修改为SELECT TOP 1...
*/
USE TEST
GO
IF OBJECT_ID('dbo.Test_TOP1') IS NOT NULL
	DROP TABLE dbo.Test_TOP1
CREATE TABLE dbo.Test_TOP1
(
	ID INT 
	,TransactionNumber CHAR(25)
	,purno CHAR(8)
)
INSERT INTO dbo.Test_TOP1(ID,TransactionNumber,purno)
SELECT 234434,'1111111111','105336'
UNION ALL
SELECT 234445,'2222222222','105336'
UNION ALL
SELECT 234345,'fdfdrynkjs','1053334'

SELECT TransactionNumber,purno 
FROM dbo.Test_TOP1 WITH (NOLOCK)

--错误的写法一
DECLARE @TransactionNumber CHAR(25)
SELECT @TransactionNumber=ISNULL(TransactionNumber,'')--BYDBA 1.变量赋值,请修改为SELECT TOP 1...
FROM [dbo].[Test_TOP1] WITH (NOLOCK)
WHERE purno='105336'  
 
SELECT @TransactionNumber

----错误的写法二
SET @TransactionNumber=	ISNULL(
								(SELECT TOP 1 TransactionNumber
								FROM [dbo].[Test_TOP1] WITH (NOLOCK)
								WHERE purno='105336' )
								,'')


SELECT @TransactionNumber

--正确的写法
SELECT TOP 1 @TransactionNumber=ISNULL(TransactionNumber,'')
FROM [dbo].[Test_TOP1] WITH (NOLOCK)
WHERE purno='105336'  

SELECT @TransactionNumber
-------------------------------------------------------------------

/*
SELECT TOP 1 ... ORDER BY与MAX，MIN
*/
SELECT TOP 1 item
FROM [SCM].[dbo].[potran01] WITH (NOLOCK)
ORDER BY item DESC

--建议使用MAX函数。
SELECT MAX(item)
FROM [SCM].[dbo].[potran01] WITH (NOLOCK)
-------------------------------------------------------------------

/*----------------------BYDBA 3.去掉RTRIM函数。--------------------
1.去掉Isnull里面的RTRIM函数
2.使用函数 LEN()的时候，建议去掉 RTRIM。
3.做字符串比较，SQLServer会忽略掉尾部的空格
*/
DECLARE @string CHAR(50)
SET @string='Test isnull  '

--Isnull
SELECT LEN(ISNULL(@string,''))
		,LEN(ISNULL(RTRIM(@string),''))

--使用Len函数
SELECT LEN(RTRIM(@string))
		,LEN(@string)

--字符串比较
IF @string<>'Test isnull'
	SELECT 'Not Equal'
ELSE IF @string='Test isnull'
	SELECT 'Equal'
ELSE
	SELECT 'Unknow'
-------------------------------------------------------------------

/*创建Index需要注意的地方。
--BYDBA 1.请按照规范对INDEX命名，IX_表名_字段名。
--BYDBA 1.创建索引时请显式申明聚集类型 NONCLUSTERED
--BYDBA 1.添加索引的时候，请指定填充因子 WITH FILLFACTOR = 90
*/
CREATE NONCLUSTERED INDEX IX_DBMOVE_Training_Trn_Date
ON dbo.DBMOVE_Training
(
	Trn_Date ASC
)WITH FILLFACTOR=80
-------------------------------------------------------------------
/*
12.禁止在使用了事务的情况下，不编写防止造成未提交或者未回滚事务的情况的处理代码
   事务要最小化
--BYDBA 1.SP中禁止使用PRINT语句。
*/
	BEGIN TRANSACTION;  
	BEGIN TRY  
	
	INSERT INTO dbo.users(id
							,name
							,Age)
	SELECT UID,U_Name,U_Age 
	FROM #Temp AS a
	WHERE NOT EXISTS(SELECT TOP 1 1
						FROM dbo.users AS b WITH (NOLOCK)
						WHERE b.id=a.UID)
		PRINT 'Insert successfully'--BYDBA 1.禁止在存储过程中输出不需要的信息
		COMMIT TRANSACTION
	END TRY
	BEGIN CATCH--BYDBA 1.请编写防止造成未提交或者未回滚事务的情况的处理代码。
	END CATCH
-------------------------------------------------------------------
/*
--BYDBA 3.请保证WHERE语句中=两边的数据类型一致，否则SQLServer走不到Index
使用显式的数据类型转换（CAST或者CONVERT）
*/

--D2WHP01

--不正确的写法
SELECT A.edate
		,B.vendno
FROM [SCM].[dbo].[arinvt01] AS A WITH (NOLOCK)
	INNER JOIN [SCM].[dbo].[potran01] AS B WITH (NOLOCK)
	ON A.Item=B.Item
WHERE B.purno=105336

--正确的写法
SELECT A.edate
		,B.vendno
FROM [SCM].[dbo].[arinvt01] AS A WITH (NOLOCK)
	INNER JOIN [SCM].[dbo].[potran01] AS B WITH (NOLOCK)
	ON A.Item=B.Item
WHERE B.purno=CONVERT(CHAR(8),105336)--B.purno的数据类型为char(8)
--或者是
SELECT A.edate
		,B.vendno
FROM [SCM].[dbo].[arinvt01] AS A WITH (NOLOCK)
	INNER JOIN [SCM].[dbo].[potran01] AS B WITH (NOLOCK)
	ON A.Item=B.Item
WHERE B.purno='105336'
-------------------------------------------------------------------
/*
14.尽量不考虑游标的使用，在必须要使用游标的情况下，
请定义游标的类型为：LOCAL STATIC FORWARD_ONLY READ_ONLY,在查询表时加WITH(NOLOCK)（并行性，读取数据的速度，消耗资源上考虑）
	LOCAL: 指定对于在其中创建的批处理、存储过程或触发器来说，该游标的作用域是局部的。
	STATIC :在对该游标进行提取操作时返回的数据中不反映对基表所做的修改，并且该游标不允许修改。
	FORWARD_ONLY :指定游标只能从第一行滚动到最后一行。
	READ_ONLY 禁止通过该游标进行更新
    查询表上加WITH(NOLOCK)：这样使本来在表上加了S锁的表不加任何锁，提高并行性，减低锁资源的消耗。
游标使用完毕后，必须关闭和释放游标资源
*/
DECLARE cur_MyCursor CURSOR LOCAL STATIC READ_ONLY FORWARD_ONLY
FOR
SELECT TOP 10 QUOTENAME(SCHEMA_NAME(schema_id),'[]')+'.'+QUOTENAME(name,'[]')
FROM sys.tables WITH (NOLOCK)

/*---------------BYDBA 3.尽量不使用游标,请使用其他的方法。----------------
1.拼接字符串
2.update/delete/insert 记录
*/

/*
15.缓存临时数据时，使用真正的临时表（#或者##开头）的表，避免将临时表缓存到正式表中
--这个会在稍后的专题Demo中有演示
*/

/*
16.动态T-SQL处理语句中，如果涉及到变量，尽量使用sp_executesql，通过参数传递进行处理，
避免使用EXEC硬拼SQL语句存储过程或函数中不允许包含未使用的参数或变量
--这个会在稍后的专题Demo中有演示
*/

-------------------------------------------------------------------
/*
17.脚本中，禁止出现对正式对象的DROP
*/
DROP PROC dbo.up_test->EXEC sp_rename 'dbo.up_test','_MarkDelete_20100520_up_test'
DROP TABlE dbo.Test->EXEC sp_rename 'dbo.Test','_MarkDelete_20100520_Test'
DROP VIEW dbo.V_Test->EXEC sp_rename 'dbo.V_Test','_MarkDelete_20100520_V_Test'

--Mark Delete
-------------------------------------------------------------------
/*
18.能使用INNER JOIN实现的处理，不要使用外连接
*/

DECLARE @t1 TABLE
(
	id INT
	,name VARCHAR(10)
)


DECLARE @t2 TABLE
(
	id INT
	,name VARCHAR(10)
)

INSERT INTO @t1
SELECT 1,'a'
UNION ALL
SELECT 2,'b'
UNION ALL
SELECT 3,'c'

INSERT INTO @t2
SELECT 4,'a'
UNION ALL
SELECT 2,'b'
UNION ALL
SELECT 5,'c'

SELECT A.id,B.name
FROM @t1 AS A
	LEFT JOIN @t2 AS B
	ON A.id=B.id AND B.name='B'
WHERE B.id IS NOT NULL

--BYDBA 3.请将Left join连接修改为Inner Join连接。（Left连接表中在Where子句中有筛选条件）。
SELECT A.id,B.name
FROM @t1 AS A
	LEFT JOIN @t2 AS B
	ON A.id=B.id 
WHERE B.name='B'

SELECT A.id,B.name
FROM @t1 AS A
	INNER JOIN @t2 AS B
	ON A.id=B.id 
WHERE B.name='B'

-------------------------------------------------------------------
/*
19.数据本身不会重复，或者不需要防止重复的UNION，改用UNION ALL
*/
DECLARE @t1 TABLE
(
	id INT
	,name VARCHAR(10)
)

DECLARE @t2 TABLE
(
	id INT
	,name VARCHAR(10)
)

INSERT INTO @t1
SELECT 1,'a'
UNION ALL
SELECT 2,'b'
UNION ALL
SELECT 3,'c'

INSERT INTO @t2
SELECT 4,'d'
UNION ALL
SELECT 2,'e'
UNION ALL
SELECT 5,'f'

SELECT name
FROM @t1
UNION
SELECT name
FROM @t2
-------------------------------------------------------------------

/*
20.使用IF语句时，如果满足条件和不满足条件都会做相应的处理，则不允许使用NOT做负向判断
*/
IF EXISTS(SELECT TOP 1 1
			FROM dbo.TableName WITH (NOLOCK)
		)
	BEGIN
		--do something
	END
ELSE
	BEGIN
		--do something
	END
-------------------------------------------------------------------
/*
产品环境Replication的连接:http://s7dbm02/sync/syn.xml
21.不允许修改 Replication Chain 源头之外的表结构及数据
*/

/*
Replication Chain：
ABS_SQL.Abs.dbo.APCHCK01->NEWSQL2.Abs.dbo.APCHCK01->S1RPT02.Act.dbo.AbsAPChck01
那么我们只允许：
*/

ALTER TABLE ABS_SQL.Abs.dbo.APCHCK01
ADD XXX DATATYPE
-------------------------------------------------------------------

/*
22.如果 Replication Chain 经过某服务器，则在该服务器上做查询时，
不允许跨服务器查询该 Replication Chain 上的相关表
*/

/*
Replication Chain：
ABS_SQL.Abs.dbo.APCHCK01->NEWSQL2.Abs.dbo.APCHCK01->S1RPT02.Act.dbo.AbsAPChck01
那么我们不允许：
*/

--In NEWSQL2
SELECT TOP 1 *
FROM ABS_SQL.Abs.dbo.APCHCK01 WITH (NOLOCK)
--或者
SELECT TOP 1 *
FROM S1RPT02.Abs.dbo.AbsAPChck01 WITH (NOLOCK)

--正确的做法是：
--In NEWSQL2

SELECT TOP 1 *
FROM Abs.dbo.APCHCK01 WITH (NOLOCK)
-------------------------------------------------------------------
/*
DECLARE
    @rows int,
    @rows_limit int,
    @row_batch int,
    @row_count int
;

SELECT
    @rows = 0,
    @rows_limit = 50000, -- 处理的最大记录数限制
    @row_batch = 1000,       -- 每批处理的记录数
    @row_count = @row_batch
;

WHILE @row_count = @row_batch
    AND @rows < @rows_limit
BEGIN;
    DELETE TOP(@row_batch) SRC
       OUTPUT deleted.*     -- 如果是数据转移, 则有OUTPUT 删除记录到目标表, 否则没有这个
           INTO target_table -- 目的表
       --OUTPUT deleted.col1, deleted.col2       -- 如果源和目标表的列序不一样, 或者只转移指定的列
       --  INTO tabger_table(
       --     col1, col2)
    FROM source_table SRC       -- 源表
    WHERE filter = 1         -- 记录处理条件
    ;
    
    SELECT
       @row_count = @@ROWCOUNT,
       @rows = @rows + @row_count
    ;
    
    WAITFOR DELAY '00:00:10';   -- 每批处理之间的延时
END;


*/

--关于Replication查询的例外
/*
在PRD上存在以下的Replication链：NEWSQL.ACT->NEWSQL2.Act->ABS_SQL.Act,
因此，请直接从ACT.dbo.Newegg_InvoiceMaster获取数据
*/
--IN ABS_SQL
select count(*)  
FROM  Newsql.ACT.dbo.Newegg_InvoiceMaster b WITH(NOLOCK)--按照我们的CheckList，此处应该写为ACT.dbo.Newegg_InvoiceMaster b WITH(NOLOCK),也就是下面一个查询 
    INNER JOIN Newsql.Fedex.dbo.v_fa_somaster a WITH(NOLOCK) --这是视图，没有相应的Replication
		ON  b.InvoiceNumber = a.InvoiceNumber                         
    WHERE (b.InvoiceDate >= '2008-8-1' AND b.InvoiceDate < getdate())                        
    AND (b.FedexShippingCharge IS NULL OR b.FedexShippingCharge = 0)  
/* time statistics
SQL Server Execution Times:
   CPU time = 0 ms,  elapsed time = 1181 ms.
*/

--修改后的写法
select count(*)
FROM  ACT.dbo.Newegg_InvoiceMaster b WITH(NOLOCK)  
    INNER JOIN Newsql.Fedex.dbo.v_fa_somaster a WITH(NOLOCK) 
		ON  b.InvoiceNumber = a.InvoiceNumber                         
    WHERE (b.InvoiceDate >= '2008-8-1' AND b.InvoiceDate < getdate())                        
    AND (b.FedexShippingCharge IS NULL OR b.FedexShippingCharge = 0) 

/* time statistics
10分钟没有出来数据
*/


/*==========================The third part:Commonly Errors=====================*/

/*
1.试图在 ON 条件中过滤不满足条件的记录:
*/

/*
USE TEST
GO
IF OBJECT_ID('dbo.T1') IS NOT NULL
	DROP TABLE dbo.T1
CREATE TABLE dbo.T1
(
ID INT
,name VARCHAR(20)
)

IF OBJECT_ID('dbo.T2') IS NOT NULL
	DROP TABLE dbo.T2
CREATE TABLE dbo.T2
(
ID INT
,name VARCHAR(20)
)
INSERT INTO dbo.T1(ID,name)
SELECT 1,'a'
UNION ALL 
SELECT 1,'B'
UNION ALL 
SELECT 2,'c'
UNION ALL 
SELECT 2,'d'
UNION ALL 
SELECT 3,'a'
UNION ALL 
SELECT 4,'h'

INSERT INTO dbo.T2(ID,name)
SELECT 1,'c'
UNION ALL 
SELECT 1,'f'
UNION ALL 
SELECT 1,'C'
UNION ALL 
SELECT 2,'g'
UNION ALL 
SELECT 2,'h'
UNION ALL 
SELECT 3,'C'
*/
--错误的写法
SELECT A.ID,A.NAME, B.ID,B.name
FROM dbo.t1 A WITH (NOLOCK)
 LEFT JOIN dbo.t2 B
  ON A.ID = B.ID 
   AND A.name = 'a'
   AND B.name = 'c'

--正确的写法
SELECT A.ID,A.NAME, B.ID,B.name
FROM dbo.t1 A WITH (NOLOCK)
 LEFT JOIN dbo.t2 B WITH (NOLOCK)
  ON A.ID = B.ID AND B.name = 'c'
WHERE A.name = 'a'


--比较以下两种写法的差异
SELECT A.ID,A.NAME, B.ID,B.name
FROM dbo.t1 A WITH (NOLOCK)
 LEFT JOIN dbo.t2 B WITH (NOLOCK)
  ON A.ID = B.ID AND B.name = 'c'


SELECT A.ID,A.NAME, B.ID,B.name
FROM dbo.t1 A WITH (NOLOCK)
 LEFT JOIN dbo.t2 B WITH (NOLOCK)
  ON A.ID = B.ID
WHERE B.name = 'c'

-------------------------------------------------------------------
/*
4.使用LEFT JOIN配合WHERE条件中，判断右边表关键值是否为NULL来查询出仅在左边表出现的记录
*/

SELECT A.*
FROM dbo.T1 AS A WITH (NOLOCK)
	LEFT JOIN dbo.T2 AS B WITH (NOLOCK)
	ON A.id=B.id
WHERE B.id IS NULL


--建议修改：
SELECT *
FROM dbo.T1 AS A WITH (NOLOCK)
WHERE NOT EXISTS(SELECT TOP 1 1
			FROM dbo.T2 AS B WITH (NOLOCK)
			WHERE B.id=A.id
			)
--或者
SELECT *
FROM dbo.T1 AS A WITH (NOLOCK)
WHERE ID NOT IN (SELECT ID
			FROM dbo.T2 AS B WITH (NOLOCK)
			)
-------------------------------------------------------------------
/*----------------------------------
--BYDBA 1.避免使用SET XACT_ABORT ON 自动回滚事务，尽量使用TRY...CATCH语句

*/

--不建议
SET XACT_ABORT ON

BEGIN
	BEGIN TRAN
		UPDATE dbo.Users
		SET Name='Test'
		WHERE ID=123456
	COMMIT
END

--建议做法
BEGIN TRY
BEGIN TRAN
		UPDATE dbo.Users
		SET Name='Test'
		WHERE ID=123456
	COMMIT
END TRY
BEGIN CATCH
	ROLLBACK
END CATCH

-------------------------------------------------------------------

/*-----------------------------------------------------------------
我们尽可能保证事务短小，不应该在整个循环使用一个事务，而是采用在每一个批次使用循环的方式

下面是一个不正确的demo
*/

CREATE PROCEDURE [dbo].[UP_EC_JOB_RecycleCrawlerVisitItem]   
AS   
  BEGIN   
      SET nocount ON;   
  declare  @Day INT;  
  SET @Day=15;  
      BEGIN TRY   
          BEGIN TRAN   
  
          WHILE EXISTS (SELECT TOP 1 1   
                        FROM   [ECommerce].[dbo].[EC_Truesight_CrawlerVisitItem] WITH(nolock)   
                        WHERE  indate < Dateadd(DAY, @Day, Getdate()))   
            BEGIN   

                DELETE TOP (1000) FROM [ECommerce].[dbo].[EC_Truesight_CrawlerVisitItem]   
                WHERE  indate < Dateadd(DAY, @Day * -1, Getdate())   
				
                IF @@ROWCOUNT < 1000   
                  BREAK;   
                WAITFOR delay '00:00:15';   
            END   
  
          COMMIT TRAN   
      END TRY   
  
      BEGIN CATCH   
          IF Xact_state() <> 0   
            BEGIN   
                ROLLBACK TRAN   
            END   
      END CATCH   
  END   



  
----------------------------------------------------------------------

/*-------------------------------------------------
--BYDBA 3.建议采用临时表，因为最少会对同一张表查询两次
并且两次查询的大部分条件相同。
--------------------------------------------------*/
--GQC:D2WHP01
SELECT *
FROM [SCM].[dbo].[arinvt01] WITH (NOLOCK)
WHERE item IN(
				SELECT item
				FROM [SCM].[dbo].[potran01] WITH (NOLOCK)
				where purno='105336')

SELECT *
FROM [SCM].[dbo].[arinvt01] WITH (NOLOCK)
WHERE item IN(
				SELECT item
				FROM [SCM].[dbo].[potran01] WITH (NOLOCK)
				where purno ='110983')

--建议做法:
IF OBJECT_ID('tempdb.dbo.#temp') IS NOT NULL
	DROP TABLE #temp

SELECT item,purno
INTO #temp
FROM [SCM].[dbo].[potran01] WITH (NOLOCK)
where purno IN ('110983','105336')

SELECT *
FROM [SCM].[dbo].[arinvt01] WITH (NOLOCK)
WHERE item IN (SELECT item
				FROM #temp
				WHERE purno='110983')

SELECT *
FROM [SCM].[dbo].[arinvt01] WITH (NOLOCK)
WHERE item IN (SELECT item
				FROM #temp
				WHERE purno='105336')

-------------------------------------------------------------------
/*----------------------------------------------------
--BYDBA 3.INNER Join连接操作,请将筛选条件与连接条件分离，
--筛选条件放到Where字句中。
---------------------------------------------------*/
SELECT A.edate
		,B.vendno
FROM [SCM].[dbo].[arinvt01] AS A WITH (NOLOCK)
	INNER JOIN [SCM].[dbo].[potran01] AS B WITH (NOLOCK)
	ON A.Item=B.Item AND B.purno='105336'

--修改为：
SELECT A.edate
		,B.vendno
FROM [SCM].[dbo].[arinvt01] AS A WITH (NOLOCK)
	INNER JOIN [SCM].[dbo].[potran01] AS B WITH (NOLOCK)
	ON A.Item=B.Item
WHERE B.purno='105336'
-------------------------------------------------------------------
/*------------------------------------------------------------------
--BYDBA 1.使用COUNT(*)判断存在与否
------------------------------------------------------------------*/

IF (SELECT COUNT(*) 
	FROM [SCM].[dbo].[VendorReturnPolicy] WITH (NOLOCK) 
	WHERE VendorNumber='20001') >0      
BEGIN      
    SELECT VendorCreditDays,VRPNumber
	FROM dbo.VendorReturnPolicy WITH (NOLOCK) 
	WHERE VRPNumber = 3279        
END

--修改为:
IF EXISTS(SELECT TOP 1 1
			FROM [SCM].[dbo].[VendorReturnPolicy] WITH (NOLOCK) 
			WHERE VendorNumber='20001'
		)
BEGIN      
    SELECT VendorCreditDays,VRPNumber
	FROM dbo.VendorReturnPolicy WITH (NOLOCK) 
	WHERE VRPNumber = 3279        
END

-------------------------------------------------------------------
/*==========================The 4th part:Suggestions=====================*/

/*
1.使用 ISNULL(Col, 0) 代替 CASE WHEN Col IS NULL THEN 0 ELSE Col END
*/
SELECT CASE 
			WHEN Col IS NULL THEN 0 
			ELSE Col 
		END AS Col
FROM dbo.T1 WITH (NOLOCK)

SELECT ISNULL(Col,0)
FROM dbo.T1 WITH (NOLOCK)

-------------------------------------------------------------------
/*
2.通过合理的方法避免在 SELECT 语句中使用 DISTINCT
*/
SELECT DISTINCT
		A.au_fname
		,A.au_lname
FROM dbo.authors AS A WITH (NOLOCK) 
	INNER JOIN dbo.titleAuthor AS T WITH (NOLOCK)
ON T.au_id = A.au_id

--避免DISTINCT的写法
SELECT au_fname
		,au_lname
FROM dbo.authors AS A WITH (NOLOCK) 
WHERE EXISTS (
				SELECT TOP 1 1
				FROM dbo.titleAuthor AS T WITH (NOLOCK)
				WHERE T.au_id = A.au_id
)


-------------------------------------------------------------------
/*
3.通过合理的方法来避免Like字句，在sql语句 WHERE 语句中使用 col LIKE '%a'
*/
/*
--S1QSQL07\D2WHP01
IF OBJECT_ID('dbo.Test_Like') IS NOT NULL
Execution time:00:01:50
*/
SELECT *--45991
FROM [SCM].[dbo].[potran01] AS B WITH (NOLOCK)
WHERE B.purno LIKE '6%'

/*
Execution time:00:01:05
*/
SELECT *--45991
FROM [SCM].[dbo].[potran01] AS B WITH (NOLOCK)
WHERE LEFT(B.purno,1)='6'


--CPU time = 1782 ms,  elapsed time = 509 ms.
SELECT COUNT(1)--45991
FROM [SCM].[dbo].[potran01] AS B WITH (NOLOCK)
WHERE B.purno LIKE '%5'

--建议修改为：
--   CPU time = 983 ms,  elapsed time = 267 ms.
SELECT COUNT(1)--45991
FROM [SCM].[dbo].[potran01] AS B WITH (NOLOCK)
WHERE RIGHT(RTRIM(B.purno),1)='5'

-------------------------------------------------------------------
/*
5.在条件中， 不要包含无意义的条件
*/
SELECT *
FROM dbo.T1 WITH (NOLOCK)
WHERE 1=1
-------------------------------------------------------------------
/*
6.删除整表数据时，建议使用 TRUNCATE TABLE
*/
DELETE FROM dbo.T1

--建议
TRUNCATE TABLE dbo.T1
-------------------------------------------------------------------
--Question:
/*
在什么情况下，只能使用DELETE而不能使用TRUNCATE呢？
*/


/*
--BYDBA 3.ntext, text, and image 数据类型不再允许使用，请用nvarchar(max), varchar(max), and varbinary(max),xml 代替。
及时在临时表，表变量也不允许使用

在 Microsoft SQL Server 的未来版本中将删除 ntext、text 和 image 数据类型。
请改用 nvarchar(max)、varchar(max) 和 varbinary(max)。
*/

/*--BYDBA 3.在CREAET Function的时候，建议显示申明表结构。
*/
-- d2WHP01
USE Inventory2005
GO
SELECT X.Item 
FROM dbo.fn_CC_ItemLatestReceivingCostQuery ('07') as X
WHERE X.Item='22-136-012'

/*
  
/*  
OBJECT:   dbo.fn_CC_ItemLatestReceivingCostQuery  
SERVER:   D2whp01  
DB:    Inventory2005  
DESCRIPTION: Query item's latest receiving cost  
CREATOR:  Wewe.J.Huang  
CREATE DATE: 2008-11-19  
SELECT Item, LatestReceivingCost  
FROM dbo.fn_CC_ItemLatestReceivingCostQuery('02')  
*/  
  
CREATE FUNCTION dbo.fn_CC_ItemLatestReceivingCostQuery_ByCherish (@warehouseNumber CHAR(15))  
RETURNS @result TABLE(Item char(25),LatestReceivingCost DECIMAL(9,2)) 
AS  
BEGIN
INSERT INTO @result(Item,LatestReceivingCost) 
 SELECT Item  
		, Cost AS LatestReceivingCost  
 FROM SCM.dbo.POReceivedDetail AS POReceiving WITH (NOLOCK)  
 WHERE EXISTS (SELECT LastPOReceiving.LatestTransactionNumber  
     FROM (SELECT Item  
       , MAX(TransactionNumber) AS LatestTransactionNumber  
       FROM SCM.dbo.POReceivedDetail WITH (NOLOCK)  
       WHERE WarehouseNumber = @warehouseNumber  
       AND Cost > 0  
       GROUP BY Item) AS LastPOReceiving  
     WHERE LastPOReceiving.LatestTransactionNumber = POReceiving.TransactionNumber)  
RETURN
END
GO

SELECT X.Item 
FROM dbo.fn_CC_ItemLatestReceivingCostQuery_ByCherish ('07') as X
WHERE X.Item='22-136-012'

drop function fn_CC_ItemLatestReceivingCostQuery_ByCherish

*/
-------------------------------------------------------------------
----========================================
--IN D2WHP02,SCM
SELECT distinct e.ReferenceSonUmber,    
            f.VendorNumber    
     FROM   dbo.ediInterchangeControlNumber a WITH (NOLOCK)    
            INNER JOIN dbo.eDigroUpControlNumber b WITH (NOLOCK)   
              ON a.InterchangeControlNumber = b.InterchangeControlNumber    
            INNER JOIN dbo.EditRanSactIonSetControlNumber c WITH (NOLOCK)      
              ON b.GroupControlNumber = c.GroupControlNumber    
            INNER JOIN DropShip.dbo.DropShipMaster d WITH (NOLOCK)      
              ON d.ReferenceSonUmber = dbo.Uf_edi_filteredifilenumer(a.[FileName])    
            INNER JOIN DropShip.dbo.DropShipTransaction e WITH (NOLOCK)      
              ON d.ReferenceSonUmber = e.ReferenceSonUmber    
            INNER JOIN CodeCenter.dbo.DropShipWarehouseMap f WITH (NOLOCK)      
              ON e.WarehouseNumber = f.WarehouseNumber    
     WHERE  (c.TransactionSetIdentIfierCode <> '850' 
				OR c.TransactionSetAcknowledgmentCode IS NULL 
				OR c.TransactionSetAcknowledgmentCode <> 'A')    
            AND d.Status <> 'V'    
            AND GETDATE() > DATEADD(HOUR, 12, d.sodate)    
     GROUP BY e.ReferenceSoNumber,f.VendorNumber    
     HAVING MAX(e.DownLoadDate) < @TOLERANCEDATE     
            AND MAX(e.DownLoadDate) >= @GIVEUPDATE

--修改为：
IF OBJECT_ID('tempdb.dbo.#temp','U') IS NOT NULL
   DROP TABLE #temp
CREATE TABLE #temp
(
ID INT IDENTITY(1,1)
,ReferenceSONumber INT
,VendorNumber CHAR(15)
,DownLoadDate DATETIME
,CONSTRAINT PK_#temp PRIMARY KEY
   (
      ID ASC
   )
)

INSERT INTO #temp(ReferenceSONumber,VendorNumber,DownLoadDate)
SELECT e.ReferenceSonUmber
      ,f.VendorNumber
      ,e.DownLoadDate
FROM DropShip.dbo.DropShipTransaction e WITH (NOLOCK)  
INNER JOIN CodeCenter.dbo.DropShipWarehouseMap f WITH (NOLOCK)  
              ON e.WarehouseNumber = f.WarehouseNumber

CREATE NONCLUSTERED INDEX IX_#temp_ReferenceSonUmber ON #temp(ReferenceSonUmber ASC)
CREATE NONCLUSTERED INDEX IX_#temp_DownLoadDate ON #temp(DownLoadDate ASC)

SELECT distinct e.ReferenceSonUmber,    
            e.VendorNumber    
     FROM   dbo.ediInterchangeControlNumber a WITH (NOLOCK)      
            INNER JOIN dbo.eDigroUpControlNumber b WITH (NOLOCK)      
              ON a.InterchangeControlNumber = b.InterchangeControlNumber   
            INNER JOIN dbo.EditRanSactIonSetControlNumber c WITH (NOLOCK)      
              ON b.GroupControlNumber = c.GroupControlNumber     
            INNER JOIN DropShip.dbo.DropShipMaster d WITH (NOLOCK)      
              ON d.ReferenceSonUmber = dbo.Uf_edi_filteredifilenumer(a.[FileName]) 
        INNER JOIN #temp AS e 
        ON d.ReferenceSonUmber = e.ReferenceSonUmber     
     WHERE  (c.TransactionSetIdentIfierCode <> '850' 
        OR c.TransactionSetAcknowledgmentCode IS NULL 
        OR c.TransactionSetAcknowledgmentCode <> 'A')    
            AND d.Status <> 'V'    
            AND GETDATE() > DATEADD(HOUR, 12, d.sodate)    
     GROUP BY e.ReferenceSoNumber,e.VendorNumber    
     HAVING MAX(e.DownLoadDate) < @TOLERANCEDATE     
            AND MAX(e.DownLoadDate) >= @GIVEUPDATE

-------------------------------------------------------------------
--BYDBA 1.请确认是否需要支持多国语言，否则采用UNICODE编码的数据类型。

--IN S7DBM01
create table dbo.TUnicoude
(id int identity(1,1)
,Name1 varchar(10)
,Name2 Nvarchar(10)
)

insert into dbo.TUnicoude
select 'dfsdsd',N'abcd'
union all
select N'测试用','测试用'
union all
select '测试用',N'测试用'

select * from dbo.TUnicoude with (nolock)

drop table dbo.TUnicoude


---------------------------------------------合并字符串
USE TEST
go
IF OBJECT_ID('tempdb.dbo.#test ') IS NOT NULL
DROP TABLE dbo.#test 

CREATE TABLE #test (id int 
                   ,value VARCHAR(100)
                   )
                   
INSERT INTO #test
SELECT 43,N'aa'
UNION ALL
SELECT 43,N'bb'
UNION ALL
SELECT 43,N'cc'
UNION ALL
SELECT 66,N'kk'
UNION ALL
SELECT 66,N'zz'

select * from #test

--方法一：
;WITH  data
AS (
          SELECT DISTINCT id FROM #test
     )
     
SELECT  id,
          col2= STUFF(REPLACE(REPLACE(CAST((SELECT value
                 FROM #test 
                 WHERE id=a.id
                 FOR XML PATH(''),TYPE ) AS NVARCHAR(max)),'</value>',''),'<value>',','),1,1,'')
FROM data a

--方法二
DECLARE @groupid INT
		,@Catalog VARCHAR(MAX)

DECLARE @T TABLE
(
	groupid INT
	,Catalog VARCHAR(MAX)
)
DECLARE cur_groupid CURSOR  LOCAL STATIC FORWARD_ONLY READ_ONLY
FOR 
SELECT DISTINCT id 
FROM #test

OPEN cur_groupid
FETCH NEXT FROM cur_groupid  INTO @groupid
WHILE @@FETCH_STATUS=0
BEGIN

SET @Catalog=''
SELECT @Catalog=@Catalog+CAST(value AS VARCHAR(8))+','
FROM #test
WHERE ID=@groupid

INSERT INTO @T
SELECT @groupid,@Catalog

FETCH NEXT FROM cur_groupid  INTO @groupid
END
CLOSE cur_groupid
DEALLOCATE cur_groupid

SELECT groupid,LEFT(Catalog,LEN(Catalog)-1) FROM @T

--------------------------------------分割字符串
USE TEST
go
IF OBJECT_ID('tempdb.dbo.#test ') IS NOT NULL
DROP TABLE dbo.#test 

CREATE TABLE #test (id int 
                   ,value VARCHAR(100)
                   )
                                      
INSERT INTO #test
SELECT 32,N'a,b,c,dd,ff'
UNION ALL
SELECT 23,N'a,kk,ll,dd,ff'

select * from #test

;WITH data
AS (
	SELECT id
			,CAST(REPLACE('<value>'+value+'</value>',',','</value><value>') 
				AS XML) as c
	FROM #test
)


SELECT a.id
		,T.C.value('(.)[1]','VARCHAR(10)')
FROM data as a
CROSS APPLY C.nodes('./value') AS T(C)


------------------------

declare @job_command nvarchar(max)
set @job_command='up_UpdateDelieverDateForCVAndABS,UP_ABSAcct_GererateOpcPackageInfo,'
if object_id('tempdb.dbo.#')is not null
	drop table #
	Create table #
	( id int identity(1,1)
		,spName sysname
	)

	DECLARE @T Table
		(jobName sysname
		,stepName sysname
		,command varchar(256)
		,CreateDate datetime)	

	;WITH data
	AS (
		SELECT CAST(REPLACE('<value>'+@job_command+'</value>',',','</value><value>') 
					AS XML) as c

	)
	INSERT INTO #
	SELECT T.C.value('(.)[1]','sysname')
	FROM data as a
	CROSS APPLY C.nodes('./value') AS T(C)

select * from #


---================================================总结的一些专题================================================

------------------------------------游标专题----------------------------------------------
/*
14.定义游标时，如果不是特别需要，使用LOCAL关键显式的将游标定义为局部游标，
尽量避免使用全局（GLOBAL，这是数据库的默认行为）游标；
没有特殊需要的话，尽量使用 FORWARD_ONLY READONLY 游标,同时尽量使用静态游标  STATIC；
避免大量数据的处理避免使用游标"
*/
DECLARE cur_MyCursor CURSOR LOCAL STATIC READ_ONLY FORWARD_ONLY
FOR
SELECT TOP 10 QUOTENAME(SCHEMA_NAME(schema_id),'[]')+'.'+QUOTENAME(name,'[]')
FROM sys.tables WITH (NOLOCK)

/*---------------BYDBA 3.尽量不使用游标,请使用其他的方法。----------------
1.拼接字符串
2.update/delete/insert 记录
*/

DECLARE @Table_name SYSNAME
		,@string NVARCHAR(2000)
DECLARE cur_MyCursor CURSOR LOCAL STATIC READ_ONLY FORWARD_ONLY
FOR
SELECT TOP 10 QUOTENAME(SCHEMA_NAME(schema_id),'[]')+'.'+QUOTENAME(name,'[]')
FROM sys.tables WITH (NOLOCK)

OPEN cur_MyCursor

FETCH NEXT FROM cur_MyCursor INTO @Table_name
WHILE @@FETCH_STATUS=0
BEGIN
	SET @string=ISNULL(@string,'')+@Table_name+','
	FETCH NEXT FROM cur_MyCursor INTO @Table_name
END
CLOSE cur_MyCursor
DEALLOCATE cur_MyCursor

PRINT @string

--是用SELECT实现。
SET @string=''

SELECT @string=ISNULL(@string,'')+QUOTENAME(SCHEMA_NAME(A.schema_id),'[]')
								+'.'+QUOTENAME(A.name,'[]')
								+','
FROM 
(
	SELECT TOP 10 schema_id,name
	FROM sys.tables WITH (NOLOCK)
)AS A

PRINT @string


------------------------------------数据缓存专题----------------------------------------------
/*
15.缓存临时数据时，使用真正的临时表（#或者##开头）的表，避免将临时表缓存到正式表中
*/

--错误的做法

SET NOCOUNT ON
DECLARE @string NVARCHAR(2000)
SET @string=''

SELECT TOP 10 QUOTENAME(SCHEMA_NAME(schema_id),'[]')+'.'+QUOTENAME(name,'[]')+',' AS Name
INTO dbo.TableName--正式表用于缓存表名。
FROM sys.tables WITH (NOLOCK)

SELECT @string=ISNULL(@string,'')+Name
FROM dbo.TableName WITH (NOLOCK)
PRINT @string
DROP TABLE dbo.TableName

----正确的做法1.
SET NOCOUNT ON
DECLARE @string NVARCHAR(2000)
SET @string=''

IF OBJECT_ID('tempdb.dbo.#temp','U') IS NOT NULL
BEGIN
	DROP TABLE #temp
END


SELECT TOP 10 QUOTENAME(SCHEMA_NAME(schema_id),'[]')+'.'+QUOTENAME(name,'[]')+',' AS Name
INTO #temp--局部临时表缓存数据
FROM sys.tables WITH (NOLOCK)

SELECT @string=ISNULL(@string,'')+Name
FROM #temp
PRINT @string
IF OBJECT_ID('tempdb.dbo.#temp','U') IS NOT NULL
BEGIN
	DROP TABLE #temp
END

----正确的做法2.
SET NOCOUNT ON
DECLARE @string NVARCHAR(2000)
SET @string=''

IF OBJECT_ID('tempdb.dbo.##temp','U') IS NOT NULL
BEGIN
	DROP TABLE ##temp
END


SELECT TOP 10 QUOTENAME(SCHEMA_NAME(schema_id),'[]')+'.'+QUOTENAME(name,'[]')+',' AS Name
INTO ##temp--全局临时表缓存数据
FROM sys.tables WITH (NOLOCK)

SELECT @string=ISNULL(@string,'')+Name
FROM ##temp
PRINT @string
IF OBJECT_ID('tempdb.dbo.##temp','U') IS NOT NULL
BEGIN
	DROP TABLE ##temp
END

----正确的做法3.
SET NOCOUNT ON
DECLARE @string NVARCHAR(2000)
SET @string=''

DECLARE @Name TABLE
(
	Id INT IDENTITY(1,1)
	,TableName SYSNAME
)

--表变量缓存数据
INSERT INTO @Name(TableName)
SELECT TOP 10 QUOTENAME(SCHEMA_NAME(schema_id),'[]')+'.'+QUOTENAME(name,'[]')+','
FROM sys.tables WITH (NOLOCK)

SELECT @string=ISNULL(@string,'')+TableName
FROM @Name
PRINT @string
--在此处讲下临时表与表变量、局部表变量和全局表变量的区别
/*
临时表与表变量
相同点：用于SQL语句中临时的缓存数据
不同点：1.临时表数据存储在物理的磁盘上(数据表存放在Tempdb中)，表变量存储在内存中；
		2.由于1，所以缓存少量数据时，表变量效率比临时表高；
		3.表变量无法创建INDEX，无法修改结构，无法跨作用域，但是临时表刚好相反；
		4.由于3，所以缓存大量数据时，使用临时表加INDEX效率表变量高；
		5.表变量不受事务的影响，临时表会受事务的影响。

局部临时表和全局临时表
它们在名称、可见性以及可用性上有区别。
本地临时表的名称以单个数字符号 (#) 打头；它们仅对当前的用户连接是可见的；当用户从 SQL Server 实例断开连接时被删除。
全局临时表的名称以两个数字符号 (##) 打头，创建后对任何用户都是可见的，当所有引用该表的用户从 SQL Server 断开连接时被删除。
*/





------------------------------------动态SQL专题----------------------------------------------
/*
16.动态T-SQL处理语句中，如果涉及到变量，尽量使用sp_executesql，通过参数传递进行处理，
避免使用EXEC硬拼SQL语句存储过程或函数中不允许包含未使用的参数或变量

*/
----------------------------------------------Demo 1

DECLARE @sql NVARCHAR(MAX)
		,@TableName SYSNAME
		,@top INT
SET @TableName=N'Test_Select'
SET @top=2
SET @sql=N'SELECT TOP '+CAST(@top AS NVARCHAR(4))+N' * 
			FROM sys.tables WITH (NOLOCK)
			WHERE name='''+@TableName+''''


EXEC(@sql)

--推荐写法
SET @sql=N'SELECT TOP (@top) * 
			FROM sys.tables WITH (NOLOCK)
			WHERE name=@TableName'
EXEC sp_executesql @sql
					,N'@TableName SYSNAME
						,@top INT'
					,@TableName
					,3
----------------------------------------------demo 2
declare @sql nvarchar(max)
       ,@type sysname
       ,@crdate datetime

set @type=N'u'
set @crdate=N'2004-10-12 17:37:17.437'
SET @sql=N'
select top 5 * 
from sys.sysobjects with (Nolock)
WHERE 1=1'

IF @type IS NOT NULL AND @type<>''
BEGIN
  SET @sql=@sql+' AND type='''+@type+''''
END
  

IF @crdate IS NOT NULL
BEGIN
  SET @sql=@sql+' AND crdate>='''+CONVERT(CHAR(20),@crdate,120)+''''
END


exec sp_executesql @sql


SET @sql=N'
select top 5 * 
from sys.sysobjects with (Nolock)
WHERE 1=1'

IF @type IS NOT NULL AND @type<>''
BEGIN
    SET @sql=@sql+' AND type=@type'
END

IF @crdate IS NOT NULL
BEGIN
    SET @sql=@sql+' AND crdate>=@crdate'
END

print @sql
exec sp_executesql @sql
                  ,N'@type sysname
                     ,@crdate datetime'
                  ,@type
                  ,@crdate

select top 5 * 
from sys.sysobjects with (Nolock)
where crdate>='2004-10-12 17:37:17.437' and type='U'


---------------------Demo3:动态SQL中的like子句中
use test
go
CREATE TABLE dbo.TestLike
(
	ID INT IDENTITY(1,1)
	,Name VARCHAR(10)
)

DECLARE @i INT
SET @i=0
WHILE @i<1000
BEGIN
	insert into dbo.TestLike
	SELECT CAST(@i+@i AS VARCHAR(10))
SET @i=@i+1
END

-----以下是测试代码
DECLARE @sql NVARCHAR(max)
		,@Like NVARCHAR(10)

SET @Like=N'76'

--以前采用拼串的处理方式(会导致SQL重编译执行串，影响性能)
SET @sql=N'SELECT * FROM dbo.TestLike WITH (NOLOCK)
			WHERE Name like ''%'+@Like+'%'''

EXEC sp_executesql @sql

--新的处理方式，把变量放到执行串中。(避免SQL重编译执行串)
SET @sql=N'SELECT * FROM dbo.TestLike WITH (NOLOCK)
			WHERE Name like @Like'
SET @Like=N'%'+@Like+'%'

EXEC sp_executesql @sql
					,N'@Like NVARCHAR(10)'
					,@Like


/*
DECLARE @sql1 NVARCHAR(max)
DECLARE @id NVARCHAR(10)
	SELECT @id='1'
SELECT @sql1=N'SELECT top(1)  * from sys.databases with(nolock) where database_id='+@id+''
EXEC (@sql1)
	SELECT @id='2'
SELECT @sql1=N'SELECT top(1)  * from sys.databases with(nolock) where database_id='+@id+''
EXEC (@sql1)
	SELECT @id='3'
SELECT @sql1=N'SELECT top(1)  * from sys.databases with(nolock) where database_id='+@id+''
EXEC (@sql1)


DECLARE @sql2 NVARCHAR(max)
SELECT @sql2=N'SELECT top(1)  * from sys.databases with(nolock) where database_id=@id'
EXEC sp_executesql @sql2,N'@id int',@id=1
EXEC sp_executesql @sql2,N'@id int',@id=2
EXEC sp_executesql @sql2,N'@id int',@id=3


--DBCC freeproccache
SELECT 
cacheobjtype,objtype,usecounts ,refcounts,sql 
 FROM sys.syscacheobjects WITH(NOLOCK)
WHERE sql NOT LIKE '%cach%' 
AND  sql LIKE '%SELECT top(1)  * from sys.databases with(nolock) where database_id%'
*/

---------------------Demo4:将动态SQL的参数放到执行串中

/*
我们知道，SQL语句在执行前首先将被编译并通过查询优化引擎进行优化，从而得到优化后的执行计划，然后按照执行计划被执行。
对于整体相似、仅仅是参数不 同的SQL语句，SQL Server可以重用执行计划。
但对于不同的SQL语句，SQL Server并不能重复使用以前的执行计划，而是需要重新编译出一个新的执行计划。
*/
DBCC FREEPROCCACHE

DECLARE @t datetime 
SET @t = getdate() 

SET NOCOUNT ON 
DECLARE @i INT
		, @count INT
		, @sql nvarchar(4000) 
;
SET @i = 20000 
WHILE @i <= 30000 
BEGIN 
	SET @sql = 'SELECT @count=count(*) FROM sys.objects WHERE object_id = ' + cast( @i as varchar(10) ) 

	EXEC sp_executesql @sql 
						,N'@count INT OUTPUT'
						, @count OUTPUT 
SET @i = @i + 1 
END 


SELECT Count(*) AS Cache_counts
		,sum(size_in_bytes)/1024./1024. AS Cache_TotalSize_MB
		,DATEDIFF( second, @t, GETDATE() ) AS Exec_internal
FROM sys.dm_exec_cached_plans 

GO
DBCC FREEPROCCACHE

DECLARE @t datetime 
SET @t = getdate() 

SET NOCOUNT ON 
DECLARE @i INT
		, @count INT
		, @sql nvarchar(4000) 
SET @i = 20000 
WHILE @i <= 30000 
BEGIN 
SET @sql = 'select @count=count(*) FROM sys.objects WHERE object_id = @i' 
EXEC sp_executesql @sql
					, N'@count int output
						, @i int'
					, @count OUTPUT
					, @i 
SET @i = @i + 1 
END 

SELECT Count(*) AS Cache_counts
		,sum(size_in_bytes)/1024./1024. AS Cache_TotalSize_MB 
		,DATEDIFF( second, @t, GETDATE() )AS Exec_internal
FROM sys.dm_exec_cached_plans 
GO


----------------比较EXEC和sp_executesql的区别


------sp_executesql 输出参数
declare @sql nvarchar(max)
       ,@type sysname
		,@countOUT INT
SET @sql='SELECT @countOUT=COUNT(1)
		FROM sys.objects WITH (NOLOCK)
		WHERE type=@type'

EXEC sp_executesql @sql
					,N'@type sysname
						,@countOUT INT OUTPUT'
					,'U'
					,@countOUT OUTPUT
select @countOUT


------sp_executesql安全和效率

--DBCC FREEPROCCACHE 
declare @sql nvarchar(max)
       ,@type sysname

SET @sql=N'
select top 5 * 
from sys.sysobjects with (Nolock)
WHERE 1=1'

--set @type=N'u'
SET @type=N'u'''+' OR 1=1;EXEC master.dbo.xp_create_subdir ''C:\temp''--'----不安全的代码

EXEC(@sql+' AND type='''+@type+'''')
print @sql+' AND type='''+@type+''''
SET @type=N'P'
EXEC(@sql+' AND type='''+@type+'''')

SET @sql=N'
select top 5 * 
from sys.sysobjects with (Nolock)
WHERE type=@type'

exec sp_executesql @sql
                  ,N'@type sysname'
                  ,'U'

exec sp_executesql @sql
                  ,N'@type sysname'
                  ,'P'


SELECT A.bucketid
		,A.refcounts
		,A.usecounts
		,A.size_in_bytes
		,A.cacheobjtype
		,B.text
FROM sys.dm_exec_cached_plans  AS A 
CROSS APPLY sys.dm_exec_sql_text(A.plan_handle) AS B
WHERE A.cacheobjtype='Compiled Plan'


--Question:
/*
1.在哪些情况下，不能使用sp_executesql传参格式？
2.Demo中的变量@sql的数据类型使用VARCHAR(MAX)可以吗？
*/


------------------------------------关于Distinct专题----------------------------------------------
USE Test
GO
/*
USE Test
GO
CREATE TABLE dbo.Test_Author
(
	ID INT IDENTITY(1,1)NOT NULL
	,Author VARCHAR(5) NULL
)
INSERT INTO dbo.Test_Author(Author)
SELECT 'John' UNION ALL
SELECT 'Jim' UNION ALL
SELECT 'Mark' UNION ALL
SELECT 'Lucy' UNION ALL
SELECT 'Mary'
GO
CREATE TABLE dbo.Test_Artiles
(
ID INT IDENTITY(1,1)NOT NULL
,AuthorID INT
,ArtilesName VARCHAR(50) NULL
)
INSERT INTO dbo.Test_Artiles(AuthorID,ArtilesName)
SELECT 1,'The world is falt' UNION ALL
SELECT 2,'Alice''s Adventures in Wonderland'UNION ALL
SELECT 2,'Animal Farm'UNION ALL
SELECT 3,'Brave New World'UNION ALL
SELECT 4,'Treasure Island'UNION ALL
SELECT 3,'The Adventures of Tom Sawyer'
*/
--找出已经出版过书作者

--不关心数据是否重复的查询请不要使用Distinct
SELECT B.Author
FROM dbo.Test_Artiles AS A WITH (NOLOCK)
	INNER JOIN dbo.Test_Author AS B WITH (NOLOCK)
	ON A.AuthorID=B.ID

--IN字句中的查询请不要使用Distinct
--错误的写法
SELECT B.Author
FROM dbo.Test_Author AS B WITH (NOLOCK)
WHERE B.ID IN(
				SELECT DISTINCT AuthorID
				FROM dbo.Test_Artiles AS A WITH (NOLOCK)
			 )

--正确的写法
SELECT B.Author
FROM dbo.Test_Author AS B WITH (NOLOCK)
WHERE B.ID IN(
				SELECT  AuthorID
				FROM dbo.Test_Artiles AS A WITH (NOLOCK)
			 )

--请不要将Distinct与UNION或者UNION ALL联合使用

--错误的写法一
SELECT DISTINCT AuthorID,Author
FROM
	(
		SELECT 1 AS AuthorID,'Jim' AS Author
		UNION ALL	
		SELECT 1,'Jim'
	) AS A

--错误的写法二
SELECT DISTINCT AuthorID,Author
FROM
	(
		SELECT 1 AS AuthorID,'Jim' AS Author
		UNION	
		SELECT 1,'Jim'
	) AS A

-- 正确的写法
SELECT AuthorID,Author
FROM
	(
		SELECT 1 AS AuthorID,'Jim' AS Author
		UNION	
		SELECT 1,'Jim'
	) AS A

--请不要把Distinct和Group By一起使用

SELECT DISTINCT B.Author
FROM dbo.Test_Artiles AS A WITH (NOLOCK)
	INNER JOIN dbo.Test_Author AS B WITH (NOLOCK)
	ON A.AuthorID=B.ID
GROUP BY B.Author

SELECT B.Author
FROM dbo.Test_Artiles AS A WITH (NOLOCK)
	INNER JOIN dbo.Test_Author AS B WITH (NOLOCK)
	ON A.AuthorID=B.ID
GROUP BY B.Author

SELECT DISTINCT B.Author
FROM dbo.Test_Artiles AS A WITH (NOLOCK)
	INNER JOIN dbo.Test_Author AS B WITH (NOLOCK)
	ON A.AuthorID=B.ID

--在某些情况下，使用子查询来代替Distinct
SELECT DISTINCT B.Author
FROM dbo.Test_Artiles AS A WITH (NOLOCK)
	INNER JOIN dbo.Test_Author AS B WITH (NOLOCK)
	ON A.AuthorID=B.ID

SELECT B.Author
FROM dbo.Test_Author AS B WITH (NOLOCK)
WHERE B.ID IN(
				SELECT AuthorID
				FROM dbo.Test_Artiles AS A WITH (NOLOCK)
			 )

--在没有聚合函数的情况下，请优先使用Distinct而不是Group By
--优先使用
SELECT DISTINCT B.Author
FROM dbo.Test_Artiles AS A WITH (NOLOCK)
	INNER JOIN dbo.Test_Author AS B WITH (NOLOCK)
	ON A.AuthorID=B.ID

--不推荐
SELECT B.Author
FROM dbo.Test_Artiles AS A WITH (NOLOCK)
	INNER JOIN dbo.Test_Author AS B WITH (NOLOCK)
	ON A.AuthorID=B.ID
GROUP BY B.Author
--在含有聚合函数的情况下，请使用Group BY而不是Distinct

SELECT B.Author,COUNT(AuthorID) AS ArtilesCount
FROM dbo.Test_Artiles AS A WITH (NOLOCK)
	INNER JOIN dbo.Test_Author AS B WITH (NOLOCK)
	ON A.AuthorID=B.ID
GROUP BY B.Author
ORDER BY ArtilesCount DESC

-----------============================== XML专题 ========================================

--BYDBA 1.涉及到xml处理时，使用sql server 2005 关于xml的处理方式

DECLARE @idoc INT
DECLARE @doc XML
SET @doc =N'
<ROOT>
<Customer CustomerID="VINET" ContactName="Paul Henriot">
   <Order OrderID="10248" CustomerID="VINET" EmployeeID="5" 
           OrderDate="1996-07-04T00:00:00">
      <OrderDetail ProductID="11" Quantity="12"/>
   </Order>
</Customer>
<Customer CustomerID="LILAS" ContactName="Carlos Gonzlez">
   <Order OrderID="10283" CustomerID="LILAS" EmployeeID="3" 
           OrderDate="1996-08-16T00:00:00">
      <OrderDetail ProductID="72" Quantity="3"/>
   </Order>
</Customer>
</ROOT>'

--SQLServer 2000对XML的处理方式.  --不在使用

--Create an internal representation of the XML document.
EXEC SP_XML_PREPAREDOCUMENT @idoc OUTPUT, @doc
-- SELECT stmt using OPENXML rowset provider
SELECT *
FROM   OPENXML (@idoc, '/ROOT/Customer/Order/OrderDetail',2)
         WITH (OrderID       INT         '../@OrderID',
               CustomerID  VARCHAR(10) '../@CustomerID',
               OrderDate   DATETIME    '../@OrderDate',
               ProdID      INT         '@ProductID',
               Qty         INT         '@Quantity')
EXEC SP_XML_REMOVEDOCUMENT @idoc


--推荐做法:
SELECT T.c.value('(./Order/@OrderID)[1]','INT')					AS OrderID
		,T.c.value('(./@CustomerID)[1]','VARCHAR(10)')			AS CustomerID
		,T.c.value('(./Order/@OrderDate)[1]','DATETIME')		AS OrderDate
		,T.c.value('(./Order/OrderDetail/@ProductID)[1]','INT') AS ProdID
		,T.c.value('(./Order/OrderDetail/@Quantity)[1]','INT')	AS Qty
FROM @doc.nodes('/ROOT/Customer') T(c)
-----------======================================================================

--BYDBA 1.XML数据类型必须是和UNICODE类型的数据相互转换。

--IN S7DBM01
DECLARE 
    @x1 xml,
    @s1 varchar(max)
    
SET @x1 = CONVERT(xml,N'<root>测试</root>')
SET @s1 = CONVERT(varchar(max),@x1)
GO

--而下面的代码,则不会出现错误:

DECLARE 
    @x1 nvarchar(max),
    @s1 nvarchar(max)
    
SET @x1 = N'<root>测试</root>'
SET @s1 = CONVERT(nvarchar(max),@x1)
GO

DECLARE 
    @x1 xml,
    @s1 nvarchar(max)  --这里定义为nvarchar
    
SET @x1 = CONVERT(xml,N'<root>测试</root>')
SET @s1 =  CONVERT(nvarchar(max),@x1)
GO
-----------======================================================================
--BYDBA 3.解析XML来进行变量赋值
declare @Message xml
		,@NameSpace INT
		,@GuidID INT
		,@Tag INT
		,@Action INT
		,@UerID INT
set @Message=N'
<Publish>
	<Node>
		<MessageHead>
			<Namespace>1</Namespace>
			<OriginalGUID>11</OriginalGUID>
			<UerID/>
		</MessageHead>			
		<MessageHead>
			<Namespace>2</Namespace>
			<OriginalGUID>22</OriginalGUID>
		</MessageHead>
	</Node>
</Publish>
'
--写法一
SELECT TOP 1  
	@NameSpace =T.c.value('(Namespace)[1]', 'INT')  
	,@GuidID = T.c.value('(OriginalGUID)[1]', 'INT')  
	,@UerID = T.c.value('(UerID)[1]', 'INT')  
 FROM @Message.nodes('/Publish/Node/MessageHead') T (c) 

select @NameSpace,@GuidID,@UerID

--写法二
DECLARE @T TABLE
(
	Namespace INT
	,OriginalGUID INT
	,UerID INT
)
INSERT INTO @T
SELECT T.c.value('(Namespace/text())[1]', 'INT')  
		,T.c.value('(OriginalGUID/text())[1]', 'INT')  
		,T.c.value('(UerID/text())[1]', 'INT')  
FROM @Message.nodes('/Publish/Node/MessageHead') T (c)

SELECT TOP 1  
		@NameSpace=  Namespace
		,@GuidID= OriginalGUID
		,@UerID = UerID
FROM @T

select @NameSpace,@GuidID,@UerID

--写法三(最优的写法)
--BYDBA 修改后的写法。两点不同：1.不需要去解析nodes 2.加上了text()函数。

DECLARE @UerID_bak INT
SELECT
	@NameSpace=@Message.value('(/Publish/Node/MessageHead/Namespace/text())[1]', 'INT')  
	,@GuidID= @Message.value('(/Publish/Node/MessageHead/OriginalGUID/text())[1]', 'INT')
	,@UerID= @Message.value('(/Publish/Node/MessageHead/UerID/text())[1]', 'INT')
	,@UerID_bak= ISNULL(@Message.value('(/Publish/Node/MessageHead/UerID/text())[1]', 'INT'),0)

select @NameSpace,@GuidID,@UerID,@UerID_bak

-- 比较 写法一,三
declare @Message xml
	,@NameSpace INT
set @Message=N'
<Publish>
	<Node>
		<MessageHead>
			<Namespace>1</Namespace>
            <Namespace>21</Namespace>
			<OriginalGUID>11</OriginalGUID>
			<UerID/>
		</MessageHead>			
		<MessageHead>
			<Namespace>2</Namespace>
			<Namespace>22</Namespace>
			<OriginalGUID>22</OriginalGUID>
		</MessageHead>
	</Node>
</Publish>
'
SELECT TOP(1) @NameSpace=T.c.value('(Namespace)[1]', 'INT')  
FROM @Message.nodes('/Publish/Node/MessageHead') T (c) 

SELECT @NameSpace=@Message.value('(/Publish/Node/MessageHead/Namespace)[1]', 'INT')  
-----------======================================================================

--BYDBA 1.多次重复定义相同的XML namespace
DECLARE @XML xml
SET @XML=N'
<Publish xmlns="http://soa.newegg.com/SOA/USA/InfrastructureService/V10/PubSubService">
  <Node>
     <UserData>TestMsg</UserData>
	 <SessionID>0xABde12345</SessionID>
	 <OrderNumber>102365</OrderNumber>
	 <CustomerNumber>236598</CustomerNumber>
  </Node>
</Publish>'

DECLARE @SessionID CHAR(15)
		,@OrderNumber INT
		,@CustomerNumber INT

	      SELECT 
                @SessionID=@XML.value( 'declare namespace NEM=''http://soa.newegg.com/SOA/USA/InfrastructureService/V10/PubSubService'';
	   ( /NEM:Publish/NEM:Node/NEM:SessionID/text())[1]','CHAR(15)'),
				-- SessionID
                @OrderNumber=@XML.value( 'declare namespace NEM=''http://soa.newegg.com/SOA/USA/InfrastructureService/V10/PubSubService'';
	   ( /NEM:Publish/NEM:Node/NEM:OrderNumber/text())[1]','int'),
                @CustomerNumber=@XML.value( 'declare namespace NEM=''http://soa.newegg.com/SOA/USA/InfrastructureService/V10/PubSubService'';
	   ( /NEM:Publish/NEM:Node/NEM:CustomerNumber/text())[1]','int')
SELECT @SessionID,@OrderNumber,@CustomerNumber

--BYDBA 修改后的写法。
;WITH XMLNAMESPACES (DEFAULT 'http://soa.newegg.com/SOA/USA/InfrastructureService/V10/PubSubService')
SELECT @SessionID=@XML.value('(/Publish/Node/SessionID/text())[1]','CHAR(15)')
		,@OrderNumber=@XML.value('(/Publish/Node/OrderNumber/text())[1]','int')
		,@CustomerNumber=@XML.value('(/Publish/Node/CustomerNumber/text())[1]','int')

SELECT @SessionID,@OrderNumber,@CustomerNumber

---多命名空间取值
DECLARE @xml1 XML
SET @xml1='
<MyProject xmlns="http://www.mysuhect.com/namespace">
  <Subject>MySubject1</Subject>
  <Subject>MySubject2</Subject>
  <Node xmlns="http://www.mysuhect.com/namespace2">kxlx
    <body>body1</body>
    <body>body2</body>
  </Node>
</MyProject>'

;WITH XMLNAMESPACES (DEFAULT 'http://www.mysuhect.com/namespace','http://www.mysuhect.com/namespace2' AS nm)
SELECT @xml1.value('(/MyProject/Subject/text())[1]','varchar(100)')
,@xml1.value('(/MyProject/nm:Node/nm:body/text())[1]','varchar(100)')
,@xml1.value('/MyProject[1]/nm:Node[1]/body[2]','varchar(100)')    --null
,@xml1.value('/MyProject[1]/nm:Node[1]/nm:body[2]','varchar(100)')



-----------======================================================================

--BYDBA 3.请指定XML的绝对路径，将'//'修改为'/'
DECLARE @XML xml
SET @XML=N'
<Publish xmlns="http://soa.newegg.com/SOA/USA/InfrastructureService/V10/PubSubService">
	<UserData>
	  <Node>
		 <UserData>TestMsg</UserData>
		 <SessionID>0xABde12345</SessionID>
		 <OrderNumber>102365</OrderNumber>
		 <CustomerNumber>236598</CustomerNumber>
	  </Node>
	</UserData>
</Publish>'

DECLARE @SessionID CHAR(15)
		,@OrderNumber INT
		,@CustomerNumber INT

--使用namspace加相对路径
;WITH XMLNAMESPACES (DEFAULT 'http://soa.newegg.com/SOA/USA/InfrastructureService/V10/PubSubService')
SELECT @SessionID=@XML.value('(//SessionID/text())[1]','CHAR(15)')
		,@OrderNumber=@XML.value('(//OrderNumber/text())[1]','int')
		,@CustomerNumber=@XML.value('(//CustomerNumber/text())[1]','int')

SELECT @SessionID,@OrderNumber,@CustomerNumber;

--解析所有namespace下的节点
SELECT @SessionID=@XML.value('(/*:Publish/*:UserData/*:Node/*:SessionID/text())[1]','CHAR(15)')
		,@OrderNumber=@XML.value('(/*:Publish/*:UserData/*:Node/*:OrderNumber/text())[1]','int')
		,@CustomerNumber=@XML.value('(/*:Publish/*:UserData/*:Node/*:CustomerNumber/text())[1]','int')

SELECT @SessionID,@OrderNumber,@CustomerNumber

--BYDBA 修改后的写法。
;WITH XMLNAMESPACES (DEFAULT 'http://soa.newegg.com/SOA/USA/InfrastructureService/V10/PubSubService')
SELECT @SessionID=@XML.value('(/Publish/UserData/Node/SessionID/text())[1]','CHAR(15)')
		,@OrderNumber=@XML.value('(/Publish/UserData/Node/OrderNumber/text())[1]','int')
		,@CustomerNumber=@XML.value('(/Publish/UserData/Node/CustomerNumber/text())[1]','int')

SELECT @SessionID,@OrderNumber,@CustomerNumber
-----------======================================================================

--使用exist()与value()时对性能的影响
DECLARE @XML xml
SET @XML=N'
<Publish xmlns="http://soa.newegg.com/SOA/USA/InfrastructureService/V10/PubSubService">
	<UserData xmlns="">
	  <Node>
		 <UserData>TestMsg</UserData>
		 <SessionID>0xABde12345</SessionID>
		 <OrderNumber>102365</OrderNumber>
		 <CustomerNumber>236598</CustomerNumber>
	  </Node>
	</UserData>
</Publish>'

--使用exist
;WITH XMLNAMESPACES(DEFAULT 'http://soa.newegg.com/SOA/USA/InfrastructureService/V10/PubSubService')
SELECT @XML.value('(/Publish/UserData/Node/UserData/text())[1]','VARCHAR(10)')
		,@XML.value('(/Publish/UserData/Node/SessionID/text())[1]','VARCHAR(10)')
		,@XML.value('(/Publish/UserData/Node/OrderNumber/text())[1]','INT')
		,@XML.value('(/Publish/UserData/Node/CustomerNumber/text())[1]','INT')
WHERE @XML.exist('/Publish/UserData/Node/OrderNumber[text()="102365"]')=1

--使用普通方法(value)
;WITH XMLNAMESPACES(DEFAULT 'http://soa.newegg.com/SOA/USA/InfrastructureService/V10/PubSubService')
SELECT @XML.value('(/Publish/UserData/Node/UserData/text())[1]','VARCHAR(10)')
		,@XML.value('(/Publish/UserData/Node/SessionID/text())[1]','VARCHAR(10)')
		,@XML.value('(/Publish/UserData/Node/OrderNumber/text())[1]','INT')
		,@XML.value('(/Publish/UserData/Node/CustomerNumber/text())[1]','INT')
WHERE @XML.value('(/Publish/UserData/Node/OrderNumber/text())[1]','INT')=102365

-----------======================================================================

--使用local-name()对性能产生的影响
DECLARE @XML xml
SET @XML=N'
<Publish xmlns="http://soa.newegg.com/SOA/USA/InfrastructureService/V10/PubSubService">
	<UserData>
	  <Node>
		 <UserData>TestMsg</UserData>
		 <SessionID>0xABde12345</SessionID>
		 <OrderNumber>102365</OrderNumber>
		 <CustomerNumber>236598</CustomerNumber>
	  </Node>
	</UserData>
</Publish>'

--解析任何命名空间对应节点值，不推荐的写法
SELECT @XML.value('(/*[local-name()="Publish"]/*[local-name()="UserData"]/*[local-name()="Node"]/*[local-name()="UserData"]/text())[1]','VARCHAR(10)')
		,@XML.value('(/*[local-name()="Publish"]/*[local-name()="UserData"]/*[local-name()="Node"]/*[local-name()="SessionID"])[1]','VARCHAR(10)')
		,@XML.value('(/*[local-name()="Publish"]/*[local-name()="UserData"]/*[local-name()="Node"]/*[local-name()="OrderNumber"])[1]','INT')
		,@XML.value('(/*[local-name()="Publish"]/*[local-name()="UserData"]/*[local-name()="Node"]/*[local-name()="CustomerNumber"])[1]','INT')

--解析特定的命名空间，推荐的写法
;WITH XMLNAMESPACES(DEFAULT 'http://soa.newegg.com/SOA/USA/InfrastructureService/V10/PubSubService')
SELECT	@XML.value('(/Publish/UserData/Node/UserData/text())[1]','VARCHAR(10)')
		,@XML.value('(/Publish/UserData/Node/SessionID/text())[1]','VARCHAR(10)')
		,@XML.value('(/Publish/UserData/Node/OrderNumber/text())[1]','INT')
		,@XML.value('(/Publish/UserData/Node/CustomerNumber/text())[1]','INT') 

-----------======================================================================

--修改XML的节点
DECLARE @POData xml
		, @InvoiceNumber int 
set @InvoiceNumber=45386473
--下面这段代码是把原来的XML里面的VendorInvoiceNumber修改为：<VendorInvoiceNumber xmlns="">45386473</VendorInvoiceNumber>
SET @POData=N'<Publish xmlns="http://soa.newegg.com/SOA/USA/InfrastructureService/V10/PubSubService">
  <Subject>POASNInternalPOInfo</Subject>
  <FromService>http://soa.newegg.com/SOA/USA/InfrastructureService/V10/NLB/PubSubService</FromService>
  <ToService>http://soa.newegg.com/SOA/USA/POASNManagement/V10/OVS02/POASNInternalPOInfoService</ToService>
  <MessageType>POV10</MessageType>
  <Node>
    <POV10>
      <Body>
        <PO>
          <VendorInvoiceNumber />
        </PO>
      </Body>
    </POV10>
  </Node>
</Publish>'

--先删除
SET @POData.modify('          
  delete /*:Publish/*:Node/*:POV10/*:Body/*:PO/*:VendorInvoiceNumber          
')    
      
--NameSpace错误的添加
--SET @POData.modify('          
--  insert <VendorInvoiceNumber>{sql:variable("@InvoiceNumber")}</VendorInvoiceNumber>          
--  into (/*:Publish/*:Node/*:POV10/*:Body/*:PO)[1]          
--')    
--
--select @POData

--正确的添加
SET @POData.modify('declare default element namespace "http://soa.newegg.com/SOA/USA/InfrastructureService/V10/PubSubService";        
  insert <VendorInvoiceNumber>{sql:variable("@InvoiceNumber")}</VendorInvoiceNumber>          
  into (/Publish/Node/POV10/Body/PO)[1]          
')    

select @POData

-----------======================================================================

--XML操作用于合并字符串
USE TEST
go
IF OBJECT_ID('tempdb.dbo.#test ') IS NOT NULL
DROP TABLE dbo.#test 

CREATE TABLE #test (id int 
                   ,value VARCHAR(100)
                   )
                   
INSERT INTO #test
SELECT 43,N'aa'
UNION ALL
SELECT 43,N'bb'
UNION ALL
SELECT 43,N'cc'
UNION ALL
SELECT 66,N'kk'
UNION ALL
SELECT 66,N'zz'

select * from #test

--方法一：
;WITH  data
AS (
          SELECT DISTINCT id FROM #test
     )
     
SELECT  id,
          col2= STUFF(REPLACE(REPLACE(CAST((SELECT value
                 FROM #test 
                 WHERE id=a.id
                 FOR XML PATH(''),TYPE ) AS NVARCHAR(max)),'</value>',''),'<value>',','),1,1,'')
FROM data a

--方法二
DECLARE @groupid INT
		,@Catalog VARCHAR(MAX)

DECLARE @T TABLE
(
	groupid INT
	,Catalog VARCHAR(MAX)
)
DECLARE cur_groupid CURSOR  LOCAL STATIC FORWARD_ONLY READ_ONLY
FOR 
SELECT DISTINCT id 
FROM #test

OPEN cur_groupid
FETCH NEXT FROM cur_groupid  INTO @groupid
WHILE @@FETCH_STATUS=0
BEGIN

SET @Catalog=''
SELECT @Catalog=@Catalog+CAST(value AS VARCHAR(8))+','
FROM #test
WHERE ID=@groupid

INSERT INTO @T
SELECT @groupid,@Catalog

FETCH NEXT FROM cur_groupid  INTO @groupid
END
CLOSE cur_groupid
DEALLOCATE cur_groupid

SELECT groupid,LEFT(Catalog,LEN(Catalog)-1) FROM @T

-----XML操作用于分割字符串
USE TEST
go
IF OBJECT_ID('tempdb.dbo.#test ') IS NOT NULL
DROP TABLE dbo.#test 

CREATE TABLE #test (id int 
                   ,value VARCHAR(100)
                   )
                                      
INSERT INTO #test
SELECT 32,N'a,b,c,dd,ff'
UNION ALL
SELECT 23,N'a,kk,ll,dd,ff'

select * from #test

;WITH data
AS (
	SELECT id
			,CAST(REPLACE('<value>'+value+'</value>',',','</value><value>') 
				AS XML) as c
	FROM #test
)


SELECT a.id
		,T.C.value('(.)[1]','VARCHAR(10)')
FROM data as a
CROSS APPLY C.nodes('./value') AS T(C)


-----------======================================================================
--XML解析中文为乱码的问题

DECLARE @xml xml 
SET @xml = 
--BYDBA 1.XML变量赋值是，必须是加N
N'
<Publish xmlns="http://soa.newegg.com/SOA/USA/InfrastructureService/V10/PubSubService">
  <Node>
    <ComReasonTreeMessage>
      <Body>        
        <ReasonDescription>测试XML解析中文为乱码的问题</ReasonDescription>
      </Body>
    </ComReasonTreeMessage>
  </Node>
</Publish>'

DECLARE @ReasonDescription1 VARCHAR(100)

;WITH XMLNAMESPACES (DEFAULT 'http://soa.newegg.com/SOA/USA/InfrastructureService/V10/PubSubService')
SELECT @ReasonDescription1=@xml.value('(Publish/Node/ComReasonTreeMessage/Body/ReasonDescription/text())[1]','VARCHAR(100)')

DECLARE @ReasonDescription NVARCHAR(100)--BYDBA 1.此处必须定义为UNICODE编码的数据类型
;WITH XMLNAMESPACES (DEFAULT 'http://soa.newegg.com/SOA/USA/InfrastructureService/V10/PubSubService')
SELECT @ReasonDescription=@xml.value('(Publish/Node/ComReasonTreeMessage/Body/ReasonDescription/text())[1]','NVARCHAR(100)')--BYDBA 1.此处必须为UNICODE编码的数据类型

select @ReasonDescription1,@ReasonDescription

---统计XML节点个数
DECLARE @xml xml
SET @xml=N'
<Publish xmlns="http://soa.newegg.com/SOA/USA/InfrastructureService/V10/PubSubService">
	<Node>
		<SubNode>Test1</SubNode>
	</Node>
	<Node>
		<SubNode>Test2</SubNode>
	</Node>
	<Node>
		<SubNode>Test3</SubNode>
	</Node>
</Publish>
'

--不推荐的写法
DECLARE @T TABLE
(
	ID INT IDENTITY(1,1)
	,Node XML
)

;WITH XMLNAMESPACES (DEFAULT 'http://soa.newegg.com/SOA/USA/InfrastructureService/V10/PubSubService')
INSERT INTO @T(Node)
SELECT  T.C.query('.')
FROM @xml.nodes('/Publish/Node') AS T(C)

select COUNT(*)
from @T
WHERE Node IS NOT NULL


--不推荐的做法
;WITH XMLNAMESPACES (DEFAULT 'http://soa.newegg.com/SOA/USA/InfrastructureService/V10/PubSubService')
SELECT COUNT(*)
FROM (
SELECT  T.C.query('.') AS Node
FROM @xml.nodes('/Publish/Node') AS T(C)
) AS A

--推荐的写法
;WITH XMLNAMESPACES (DEFAULT 'http://soa.newegg.com/SOA/USA/InfrastructureService/V10/PubSubService')
SELECT @xml.value('count(/Publish/Node)','INT')

---------------------------------------===========================打包XML
DECLARE @xml xml 
          ,@mess NVARCHAR(max)
 
If object_id('tempdb.dbo.#','u') is not null
	drop table #

create table #
(
id int identity(1,1)
,item varchar(10)
,Warehouse CHAR(10)
,Qty int
) 

INSERT INTO #
SELECT '100-02-001','01',1
union all
select '102-02-002','02',2
union all
select '103-02-003','03',3
union all
select '104-02-004','04',4


-------------------------不推荐的写法

Declare @Node NVARCHAR(max)
		,@body NVARCHAR(max)

SET @Node =CAST((SELECT 'pdateInventory' AS "Action"
              ,NULL AS "Comment"
               ,'Newegg.EC.USA.InventoryManagement.Deduct.V10' AS "Namespace"
              ,'Inventory,EDI' AS "Tag"
              ,'NESO' AS "Sender"
              ,'EN' AS "Languag"
              ,'1003' AS "CompanyCode"
              ,'1.0' AS "Version"
			FOR XML PATH('MessageHead'),TYPE ) AS NVARCHAR(max))

SET @body = CAST( (SELECT TOP 3  a.item AS "@ItemNumber"
															,a.Warehouse AS "@WarehouseNumber"
															,a.Qty AS "@Quantity"
										   FROM # AS A
										  FOR XML PATH('ItemInfo'),ROOT('Items'),TYPE
					)
				 AS NVARCHAR(max))

SET @mess = N'
<Publish xmlns="http://soa.newegg.com/SOA/USA/InfrastructureService/V10/EcommercePubSubService">
  <Subject>SSL11InventoryDeduction</Subject>
  <FromService>http://soa.newegg.com/SOA/USA/InfrastructureService/V10/Ecommerce/PubSubService</FromService>
  <ToService>http://soa.newegg.com/SOA/USA/OrderManagement/V10/SSL11/InventoryDeductionSSLService</ToService>
	<Node>' + @Node + 
	'<Body>
		<InventoryDeductionInfo>' + @body +
	'</InventoryDeductionInfo>
		</Body>
		</Node>
	</Publish>'

SET @xml = CAST(@mess as xml)

SELECT @xml
--------------------------------------------
select [Node/UserData/PMCData] = (        
              SELECT
                  TransactionNumber,        
                  RequestMessage = 'a',        
                  InvalidReason,
                  InDate        
              FROM dbo.InvalidRequest WITH(NOLOCK)        
              WHERE TransactionNumber<=100362          
              FOR XML RAW('InvalidRequest'), type
           )        
    FOR XML PATH ('Publish'),TYPE 
-------------------------不建议的写法
;WITH XMLNAMESPACES(DEFAULT 'http://soa.newegg.com/SOA/USA/InfrastructureService/V10/EcommercePubSubService')
 SELECT 
   'SSL11InventoryDeduction' AS "Subject"
  ,'http://soa.newegg.com/SOA/USA/InfrastructureService/V10/Ecommerce/PubSubService' AS "FromService"
  ,'http://soa.newegg.com/SOA/USA/OrderManagement/V10/SSL11/InventoryDeductionSSLService' AS "ToService"
  ,[*] =( SELECT [*] = (SELECT 'pdateInventory' AS "Action"
                                      ,NULL AS "Comment"
                                       ,'Newegg.EC.USA.InventoryManagement.Deduct.V10' AS "Namespace"
                                      ,'Inventory,EDI' AS "Tag"
                                      ,'NESO' AS "Sender"
                                      ,'EN' AS "Languag"
                                      ,'1003' AS "CompanyCode"
                                      ,'1.0' AS "Version"
                         FOR XML PATH('MessageHead'),TYPE 
						)
				  ,[*] = (SELECT [*] =(
										   SELECT TOP 3  a.item AS "@ItemNumber"
															,a.Warehouse AS "@WarehouseNumber"
															,a.Qty AS "@Quantity"
										   FROM # AS A
										  FOR XML PATH('ItemInfo'),ROOT('Items'),TYPE
										 )
						  FOR XML PATH('InventoryDeductionInfo'),ROOT('Body'),TYPE           
						   )            
			FOR XML PATH('Node'),TYPE 
		)
FOR XML PATH ('Publish'),TYPE
-------------------------建议的写法
;WITH XMLNAMESPACES(DEFAULT 'http://soa.newegg.com/SOA/USA/InfrastructureService/V10/EcommercePubSubService')
SELECT 
	'SSL11InventoryDeduction' AS "Subject"
	  ,'http://soa.newegg.com/SOA/USA/InfrastructureService/V10/Ecommerce/PubSubService' AS "FromService"
	  ,'http://soa.newegg.com/SOA/USA/OrderManagement/V10/SSL11/InventoryDeductionSSLService' AS "ToService"
	,[Node] =
		(SELECT 'pdateInventory' AS "Action"
                      ,NULL AS "Comment"
                       ,'Newegg.EC.USA.InventoryManagement.Deduct.V10' AS "Namespace"
                      ,'Inventory,EDI' AS "Tag"
                      ,'NESO' AS "Sender"
                      ,'EN' AS "Languag"
                      ,'1003' AS "CompanyCode"
                      ,'1.0' AS "Version"
         FOR XML PATH('MessageHead'),TYPE 
		)
	,[Node/Body/InventoryDeductionInfo] = 
		(
			SELECT TOP 3  a.item AS "@ItemNumber"
						,a.Warehouse AS "@WarehouseNumber"
						,a.Qty AS "@Quantity"
			FROM # AS A
			FOR XML PATH('ItemInfo'),ROOT('Items'),TYPE
		)

FOR XML PATH ('Publish'),TYPE 


--------------------------循环处理XML的性能问题
declare @Message xml
set @Message=N'
<Publish xmlns="http://soa.newegg.com/SOA/USA/InfrastructureService/V10/PubSubService">
	<Node>
		<MessageHead>
			<Sequence>1000</Sequence>
			<OriginalGUID>10000</OriginalGUID>
		</MessageHead>			
		<MessageHead>
			<Sequence>1001</Sequence>
			<OriginalGUID>10001</OriginalGUID>
		</MessageHead>
		<MessageHead>
			<Sequence>1002</Sequence>
			<OriginalGUID>10002</OriginalGUID>
		</MessageHead>
		<MessageHead>
			<Sequence>1003</Sequence>
			<OriginalGUID>10003</OriginalGUID>
		</MessageHead>
		<MessageHead>
			<Sequence>1004</Sequence>
			<OriginalGUID>10004</OriginalGUID>
		</MessageHead>
	</Node>
</Publish>
'

declare @NodeCount INT
		,@Sequence INT
		,@OriginalGUID INT

;WITH XMLNAMESPACES(DEFAULT 'http://soa.newegg.com/SOA/USA/InfrastructureService/V10/PubSubService')
SELECT @NodeCount = @Message.value(N'count(Publish/Node/MessageHead)','INT')      

IF OBJECT_ID('tempdb.dbo.#temp') IS NOT NULL
DROP TABLE #temp
CREATE TABLE #temp
(
	Sequence INT
	,OriginalGUID INT
)

INSERT INTO #temp
SELECT 1003,10003
UNION ALL
SELECT 1004,10004
UNION ALL
SELECT 1008,10008

SELECT * FROM #temp


WHILE (@NodeCount > 0)
BEGIN
		;with xmlnamespaces(default 'http://soa.newegg.com/SOA/USA/InfrastructureService/V10/PubSubService')
		SELECT
			@Sequence = @Message.value('(/Publish/Node/MessageHead[sql:variable("@NodeCount")]/Sequence/text())[1]', 'INT')  
		   ,@OriginalGUID = @Message.value('(/Publish/Node/MessageHead[sql:variable("@NodeCount")]/OriginalGUID/text())[1]', 'INT')  

		DELETE 
		FROM #temp
		WHERE Sequence = @Sequence
				AND OriginalGUID = @OriginalGUID

SET @NodeCount = @NodeCount - 1
END


SELECT * FROM #temp 

-----------------------------------建议的处理方式

IF OBJECT_ID('tempdb.dbo.#temp1') IS NOT NULL
DROP TABLE #temp1
CREATE TABLE #temp1
(
	Sequence INT
	,OriginalGUID INT
)

INSERT INTO #temp1
SELECT 1003,10003
UNION ALL
SELECT 1004,10004
UNION ALL
SELECT 1008,10008

SELECT * FROM #temp1



DECLARE @T TABLE
(
	Sequence INT
	,OriginalGUID INT
)

;WITH XMLNAMESPACES(DEFAULT 'http://soa.newegg.com/SOA/USA/InfrastructureService/V10/PubSubService')
INSERT INTO @T
SELECT T.C.value('(./Sequence/text())[1]','INT')
		,T.C.value('(./OriginalGUID/text())[1]','INT')
FROM @Message.nodes('Publish/Node/MessageHead') AS T(C)

DELETE A
FROM #temp1 AS A
	INNER JOIN @T AS B
	ON A.Sequence = B.Sequence
				AND A.OriginalGUID = B.OriginalGUID

SELECT * FROM #temp1

---------------------------------------处理xml节点解析的问题
declare @messageBody xml

set @messageBody = N'
<Publish xmlns="http://soa.newegg.com/SOA/CN/InfrastructureService/V10/NeweggCNPubSubService">
      <Subject>WH49TMSFeedback</Subject>
		 <FromService>http://soa.newegg.com/SOA/CN/OrderManagement/V10/Warehouse49/TMSFeedback</FromService>
		 <ToService>http://soa.newegg.com/SOA/CN/InfrastructureService/V10/NeweggCN/PubSubService</ToService>
		<Node>
             <Root>TestMsg</Root>
          </Node>
    </Publish>'

select CAST(REPLACE(CAST(@messageBody AS nvarchar(MAX))
,'xmlns="http://soa.newegg.com/SOA/CN/InfrastructureService/V10/NeweggCNPubSubService"','') as xml).query('/Publish/Node/Root')  --SSBRouter方式

--正确的写法
select @messageBody.query('declare default element namespace "http://soa.newegg.com/SOA/CN/InfrastructureService/V10/NeweggCNPubSubService" 
;/Publish/Node/Root')

------------------------拼接XML
DECLARE @tb_Test TABLE
(
	ID INT IDENTITY(1,1)
	,WarehouseTo CHAR(2)
	,CompanyID INT
	,OrderType INT
	,ASNNumber varchar(20)
	,VendorID INT
	,InternalMemo VARCHAR(max)
)
INSERT INTO @tb_Test
SELECT '07'
		,123456
		,106
		,'1016507'
		,258
		,''

DECLARE @RejectWarehouse varchar(10)
        DECLARE @CompanyID int
        DECLARE @OrderType int
        DECLARE @ASNNumber varchar(20)
        DECLARE @VendorID int
        DECLARE @RejectMemo varchar(max)

SELECT TOP 1 
            @RejectWarehouse = RTRIM(WarehouseTo),
            @CompanyID = CompanyID,
            @OrderType = OrderType,
            @ASNNumber = RTRIM(ASNNumber),
            @VendorID = VendorID,
            @RejectMemo = RTRIM(ISNULL(InternalMemo, ''))
FROM @tb_Test
-------------------
--BYCherish:拼接字符串的写法
DECLARE @RejectMsg varchar(max)
        SET @RejectMsg = '
   <Publish xmlns="http://soa.newegg.com/SOA/USA/InfrastructureService/V10/PubSubService">
	<Subject>GoodReceiptWH' + @RejectWarehouse + '</Subject>
	<FromService>http://soa.newegg.com/SOA/USA/InfrastructureService/V10/NLB/PubSubService</FromService>
	<ToService>http://soa.newegg.com/SOA/USA/POASNManagement/V11/Warehouse' + @RejectWarehouse + '/GoodsReceiptSSBService</ToService>
	<Node>
		<MessageHead>
			<Namespace>http://soa.newegg.com/POASNManagement/RTRejection/v10/</Namespace>
			<Version>1.0</Version>
			<Action>Reject</Action>
			<Type/>
			<Sender>' + @RejectWarehouse + '</Sender>
			<CompanyCode>' + CAST(@CompanyID AS varchar(10)) + '</CompanyCode>
		</MessageHead>
		<Body>
			<ASNOrder>
				<CompanyID>' + CAST(@CompanyID AS varchar(10)) + '</CompanyID>
				<OrderType>' + CAST(@OrderType AS varchar(10)) + '</OrderType>
				<ASNNumber>' + @ASNNumber + '</ASNNumber>
				<VendorID>' + CAST(@VendorID AS varchar(10)) + '</VendorID>
				<WarehouseNumber>' + @RejectWarehouse + '</WarehouseNumber>
				<RejectMemo>' + @RejectMemo + '</RejectMemo>
			</ASNOrder>
		</Body>
	</Node>
</Publish>'

	    DECLARE @RejectMsgXML xml
	    SET @RejectMsgXML = CAST(@RejectMsg AS xml)
select @RejectMsgXML
-------------------
--BYCherish：推荐的写法
;WITH XMLNAMESPACES(DEFAULT 'http://soa.newegg.com/SOA/USA/InfrastructureService/V10/PubSubService')
SELECT 
		'GoodReceiptWH' + @RejectWarehouse AS "Subject"
		,'http://soa.newegg.com/SOA/USA/InfrastructureService/V10/NLB/PubSubService' AS "FromService"
		,'http://soa.newegg.com/SOA/USA/POASNManagement/V11/Warehouse' + @RejectWarehouse + '/GoodsReceiptSSBService' AS "ToService"
		,[Node] = (
			SELECT 
				'http://soa.newegg.com/POASNManagement/RTRejection/v10/' AS "Namespace"
				,'1.0' AS "Version"
				,'Reject' AS "Action"
				,'' AS "Type"
				,@RejectWarehouse AS "Sender"
				,@CompanyID AS "CompanyCode"
				FOR XML PATH('MessageHead'),TYPE
		 )
		,[Node/Body]= 
			(
			SELECT 
					@CompanyID AS "CompanyID",
					@OrderType AS "OrderType",
					@ASNNumber AS "ASNNumber",
					@VendorID AS "VendorID",
					@RejectWarehouse AS "WarehouseNumber",
					@RejectMemo AS "RejectMemo"
			FOR XML PATH('ASNOrder'),TYPE
			)
FOR XML PATH('Publish')