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
1.T-SQL�ű��б�Ҫ�������ͻ��У������νṹ������
һ�е���󳤶�һ�㲻Ҫ87���ַ�������ʹ��ͳһ�ķ��
���磺���ʹ�ÿո���Ϊ������������ʹ��TAB����������
*/
USE Test
GO
--����ĸ�ʽ
DECLARE @TransactionNumber INT
		,@purno CHAR(8)
SET @purno='105336'
IF ISNULL(@purno,'')<>''
BEGIN
SELECT TOP 1 @TransactionNumber=TransactionNumber FROM [SCM].[dbo].[potran01] WITH (NOLOCK) WHERE purno=@purno
END

--��ȷ�ĸ�ʽ:
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
2.�ű��е����йؼ��֡�ϵͳ��������ϵͳ������ȫ����д
�����Բο�һ��SQL���������ж��ڸùؼ�������ʱʹ�õ��Ǵ�д����Сд��
*/

--����д��
select case @@servicename
			when 'mssqlserver' then @@servername
			else @@servicename
		end as InstanceName

--��ȷд��
SELECT CASE @@SERVICENAME
			WHEN 'MSSQLSERVER' THEN @@SERVERNAME
			ELSE @@SERVICENAME
		END AS InstanceName
-------------------------------------------------------------------

/*�������޸���ʽ��������ӱ�Ҫ��ע��
�洢���̡���ͼ���û����庯���к����ע�ͣ����ٰ����������ˡ�
�������ڡ��޸��ˡ��޸����ڡ���������������˵����
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
--BYDBA 1.���ڴ���Ŀ�ʼ�����USE GOָ��
*/
USE DB
GO
-------------------------------------------------------------------
/*
--BYDBA 1.��ָ������ļܹ���(SQL2K�г�Ϊ������)��һ�㶼��dbo��
*/
SELECT C1,C2 
FROM dbo.Test WITH (NOLOCK)
-------------------------------------------------------------------
/*
--BYDBA 1.�Ѿ������ݿ��У����üӿ�����
*/
USE Test
GO
CREATE VIEW dbo.V_TestView
AS
SELECT C1,C2
FROM [Test].[dbo].[Test] WITH (NOLOCK)--BYDBA 1.�Ѿ������ݿ��У����üӿ�����
-------------------------------------------------------------------

/*
--BYDBA 1.����ʾ���б��ֶΡ�
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

--���淶��д��
SELECT * 
FROM dbo.Test_SELECT WITH (NOLOCK)

--�Ƽ���д��
SELECT ID
		, SONumber
		, CustomerNumber
		, ShippingCode 
FROM dbo.Test_SELECT WITH (NOLOCK)
-------------------------------------------------------------------
/*
--BYDBA 1.��ָ���ֶεı������
*/

--���淶��д��
SELECT edate				--BYDBA 1.��ָ���ֶεı������
		,ISNULL(vendno,'')	--BYDBA 1.��ָ���ֶεı������
FROM [SCM].[dbo].[arinvt01] AS A WITH (NOLOCK)
	INNER JOIN [SCM].[dbo].[potran01] AS B WITH (NOLOCK)
	ON A.Item=B.Item
WHERE purno='105336'--BYDBA 1.��ָ���ֶεı������

--�Ƽ���д��
SELECT A.edate
		,ISNULL(B.vendno,'')
FROM [SCM].[dbo].[arinvt01] AS A WITH (NOLOCK)
	INNER JOIN [SCM].[dbo].[potran01] AS B WITH (NOLOCK)
	ON A.Item=B.Item
WHERE A.purno='105336'
-------------------------------------------------------------------
/*
6.��INSERT����У�����ָ�������е��б�, ����Ļ�, 
��ṹ���в���ͻᵼ�²���ʧ�ܻ��߲��뵽����ȷ������
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

--�����д��
INSERT INTO dbo.Test_INSERT--BYDBA 1.����ʾָ�������ơ�
SELECT *
FROM dbo.Test_INSERT1 AS A WITH (NOLOCK)
WHERE NOT EXISTS(SELECT TOP 1 1
				FROM dbo.Test_INSERT AS B WITH (NOLOCK)
				WHERE A.ID=B.ID)
GO

--�޸ı�
ALTER TABLE Test_INSERT
ADD ShippingCode CHAR(15)
GO

--�ٴβ������ݱ���
INSERT INTO dbo.Test_INSERT
SELECT *
FROM dbo.Test_INSERT1 AS A WITH (NOLOCK)
WHERE NOT EXISTS(SELECT TOP 1 1
				FROM dbo.Test_INSERT AS B WITH (NOLOCK)
				WHERE A.ID=B.ID)
GO


--��ȷ��д��Ϊ:
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
7.����SELECT���漰�ı����ͼ���ڷ�������ر��������Ҫ����������У�
ʹ��TABLE Hints��WITH(NOLOCK)
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

--��������ֻ࣬�ܶ�ȡ�Ѿ��ύ������
SELECT ID,NAME
FROM dbo.Test_NOLOCK

--���Զ�ȡû���ύ������
SELECT ID,NAME
FROM dbo.Test_NOLOCK WITH (NOLOCK)

-------------------------------------------------------------------
--BYDBA 1.��ʹ��WITH(NOLOCK)������NOLOCK��
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
8.�Ӳ�ѯ�У�ֻ��ѯ��������У���Ҫ�����봦�������޹ص���
*/
--�����д��
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

--��ȷ��д��
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
--BYDBA 1.������ֵ,���޸�ΪSELECT TOP 1...
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

--�����д��һ
DECLARE @TransactionNumber CHAR(25)
SELECT @TransactionNumber=ISNULL(TransactionNumber,'')--BYDBA 1.������ֵ,���޸�ΪSELECT TOP 1...
FROM [dbo].[Test_TOP1] WITH (NOLOCK)
WHERE purno='105336'  
 
SELECT @TransactionNumber

----�����д����
SET @TransactionNumber=	ISNULL(
								(SELECT TOP 1 TransactionNumber
								FROM [dbo].[Test_TOP1] WITH (NOLOCK)
								WHERE purno='105336' )
								,'')


SELECT @TransactionNumber

--��ȷ��д��
SELECT TOP 1 @TransactionNumber=ISNULL(TransactionNumber,'')
FROM [dbo].[Test_TOP1] WITH (NOLOCK)
WHERE purno='105336'  

SELECT @TransactionNumber
-------------------------------------------------------------------

/*
SELECT TOP 1 ... ORDER BY��MAX��MIN
*/
SELECT TOP 1 item
FROM [SCM].[dbo].[potran01] WITH (NOLOCK)
ORDER BY item DESC

--����ʹ��MAX������
SELECT MAX(item)
FROM [SCM].[dbo].[potran01] WITH (NOLOCK)
-------------------------------------------------------------------

/*----------------------BYDBA 3.ȥ��RTRIM������--------------------
1.ȥ��Isnull�����RTRIM����
2.ʹ�ú��� LEN()��ʱ�򣬽���ȥ�� RTRIM��
3.���ַ����Ƚϣ�SQLServer����Ե�β���Ŀո�
*/
DECLARE @string CHAR(50)
SET @string='Test isnull  '

--Isnull
SELECT LEN(ISNULL(@string,''))
		,LEN(ISNULL(RTRIM(@string),''))

--ʹ��Len����
SELECT LEN(RTRIM(@string))
		,LEN(@string)

--�ַ����Ƚ�
IF @string<>'Test isnull'
	SELECT 'Not Equal'
ELSE IF @string='Test isnull'
	SELECT 'Equal'
ELSE
	SELECT 'Unknow'
-------------------------------------------------------------------

/*����Index��Ҫע��ĵط���
--BYDBA 1.�밴�չ淶��INDEX������IX_����_�ֶ�����
--BYDBA 1.��������ʱ����ʽ�����ۼ����� NONCLUSTERED
--BYDBA 1.���������ʱ����ָ��������� WITH FILLFACTOR = 90
*/
CREATE NONCLUSTERED INDEX IX_DBMOVE_Training_Trn_Date
ON dbo.DBMOVE_Training
(
	Trn_Date ASC
)WITH FILLFACTOR=80
-------------------------------------------------------------------
/*
12.��ֹ��ʹ�������������£�����д��ֹ���δ�ύ����δ�ع����������Ĵ������
   ����Ҫ��С��
--BYDBA 1.SP�н�ֹʹ��PRINT��䡣
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
		PRINT 'Insert successfully'--BYDBA 1.��ֹ�ڴ洢�������������Ҫ����Ϣ
		COMMIT TRANSACTION
	END TRY
	BEGIN CATCH--BYDBA 1.���д��ֹ���δ�ύ����δ�ع����������Ĵ�����롣
	END CATCH
-------------------------------------------------------------------
/*
--BYDBA 3.�뱣֤WHERE�����=���ߵ���������һ�£�����SQLServer�߲���Index
ʹ����ʽ����������ת����CAST����CONVERT��
*/

--D2WHP01

--����ȷ��д��
SELECT A.edate
		,B.vendno
FROM [SCM].[dbo].[arinvt01] AS A WITH (NOLOCK)
	INNER JOIN [SCM].[dbo].[potran01] AS B WITH (NOLOCK)
	ON A.Item=B.Item
WHERE B.purno=105336

--��ȷ��д��
SELECT A.edate
		,B.vendno
FROM [SCM].[dbo].[arinvt01] AS A WITH (NOLOCK)
	INNER JOIN [SCM].[dbo].[potran01] AS B WITH (NOLOCK)
	ON A.Item=B.Item
WHERE B.purno=CONVERT(CHAR(8),105336)--B.purno����������Ϊchar(8)
--������
SELECT A.edate
		,B.vendno
FROM [SCM].[dbo].[arinvt01] AS A WITH (NOLOCK)
	INNER JOIN [SCM].[dbo].[potran01] AS B WITH (NOLOCK)
	ON A.Item=B.Item
WHERE B.purno='105336'
-------------------------------------------------------------------
/*
14.�����������α��ʹ�ã��ڱ���Ҫʹ���α������£�
�붨���α������Ϊ��LOCAL STATIC FORWARD_ONLY READ_ONLY,�ڲ�ѯ��ʱ��WITH(NOLOCK)�������ԣ���ȡ���ݵ��ٶȣ�������Դ�Ͽ��ǣ�
	LOCAL: ָ�����������д������������洢���̻򴥷�����˵�����α���������Ǿֲ��ġ�
	STATIC :�ڶԸ��α������ȡ����ʱ���ص������в���ӳ�Ի����������޸ģ����Ҹ��α겻�����޸ġ�
	FORWARD_ONLY :ָ���α�ֻ�ܴӵ�һ�й��������һ�С�
	READ_ONLY ��ֹͨ�����α���и���
    ��ѯ���ϼ�WITH(NOLOCK)������ʹ�����ڱ��ϼ���S���ı����κ�������߲����ԣ���������Դ�����ġ�
�α�ʹ����Ϻ󣬱���رպ��ͷ��α���Դ
*/
DECLARE cur_MyCursor CURSOR LOCAL STATIC READ_ONLY FORWARD_ONLY
FOR
SELECT TOP 10 QUOTENAME(SCHEMA_NAME(schema_id),'[]')+'.'+QUOTENAME(name,'[]')
FROM sys.tables WITH (NOLOCK)

/*---------------BYDBA 3.������ʹ���α�,��ʹ�������ķ�����----------------
1.ƴ���ַ���
2.update/delete/insert ��¼
*/

/*
15.������ʱ����ʱ��ʹ����������ʱ��#����##��ͷ���ı����⽫��ʱ���浽��ʽ����
--��������Ժ��ר��Demo������ʾ
*/

/*
16.��̬T-SQL��������У�����漰������������ʹ��sp_executesql��ͨ���������ݽ��д���
����ʹ��EXECӲƴSQL���洢���̻����в��������δʹ�õĲ��������
--��������Ժ��ר��Demo������ʾ
*/

-------------------------------------------------------------------
/*
17.�ű��У���ֹ���ֶ���ʽ�����DROP
*/
DROP PROC dbo.up_test->EXEC sp_rename 'dbo.up_test','_MarkDelete_20100520_up_test'
DROP TABlE dbo.Test->EXEC sp_rename 'dbo.Test','_MarkDelete_20100520_Test'
DROP VIEW dbo.V_Test->EXEC sp_rename 'dbo.V_Test','_MarkDelete_20100520_V_Test'

--Mark Delete
-------------------------------------------------------------------
/*
18.��ʹ��INNER JOINʵ�ֵĴ�����Ҫʹ��������
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

--BYDBA 3.�뽫Left join�����޸�ΪInner Join���ӡ���Left���ӱ�����Where�Ӿ�����ɸѡ��������
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
19.���ݱ������ظ������߲���Ҫ��ֹ�ظ���UNION������UNION ALL
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
20.ʹ��IF���ʱ��������������Ͳ�����������������Ӧ�Ĵ���������ʹ��NOT�������ж�
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
��Ʒ����Replication������:http://s7dbm02/sync/syn.xml
21.�������޸� Replication Chain Դͷ֮��ı�ṹ������
*/

/*
Replication Chain��
ABS_SQL.Abs.dbo.APCHCK01->NEWSQL2.Abs.dbo.APCHCK01->S1RPT02.Act.dbo.AbsAPChck01
��ô����ֻ����
*/

ALTER TABLE ABS_SQL.Abs.dbo.APCHCK01
ADD XXX DATATYPE
-------------------------------------------------------------------

/*
22.��� Replication Chain ����ĳ�����������ڸ÷�����������ѯʱ��
��������������ѯ�� Replication Chain �ϵ���ر�
*/

/*
Replication Chain��
ABS_SQL.Abs.dbo.APCHCK01->NEWSQL2.Abs.dbo.APCHCK01->S1RPT02.Act.dbo.AbsAPChck01
��ô���ǲ�����
*/

--In NEWSQL2
SELECT TOP 1 *
FROM ABS_SQL.Abs.dbo.APCHCK01 WITH (NOLOCK)
--����
SELECT TOP 1 *
FROM S1RPT02.Abs.dbo.AbsAPChck01 WITH (NOLOCK)

--��ȷ�������ǣ�
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
    @rows_limit = 50000, -- ���������¼������
    @row_batch = 1000,       -- ÿ������ļ�¼��
    @row_count = @row_batch
;

WHILE @row_count = @row_batch
    AND @rows < @rows_limit
BEGIN;
    DELETE TOP(@row_batch) SRC
       OUTPUT deleted.*     -- ���������ת��, ����OUTPUT ɾ����¼��Ŀ���, ����û�����
           INTO target_table -- Ŀ�ı�
       --OUTPUT deleted.col1, deleted.col2       -- ���Դ��Ŀ��������һ��, ����ֻת��ָ������
       --  INTO tabger_table(
       --     col1, col2)
    FROM source_table SRC       -- Դ��
    WHERE filter = 1         -- ��¼��������
    ;
    
    SELECT
       @row_count = @@ROWCOUNT,
       @rows = @rows + @row_count
    ;
    
    WAITFOR DELAY '00:00:10';   -- ÿ������֮�����ʱ
END;


*/

--����Replication��ѯ������
/*
��PRD�ϴ������µ�Replication����NEWSQL.ACT->NEWSQL2.Act->ABS_SQL.Act,
��ˣ���ֱ�Ӵ�ACT.dbo.Newegg_InvoiceMaster��ȡ����
*/
--IN ABS_SQL
select count(*)  
FROM  Newsql.ACT.dbo.Newegg_InvoiceMaster b WITH(NOLOCK)--�������ǵ�CheckList���˴�Ӧ��дΪACT.dbo.Newegg_InvoiceMaster b WITH(NOLOCK),Ҳ��������һ����ѯ 
    INNER JOIN Newsql.Fedex.dbo.v_fa_somaster a WITH(NOLOCK) --������ͼ��û����Ӧ��Replication
		ON  b.InvoiceNumber = a.InvoiceNumber                         
    WHERE (b.InvoiceDate >= '2008-8-1' AND b.InvoiceDate < getdate())                        
    AND (b.FedexShippingCharge IS NULL OR b.FedexShippingCharge = 0)  
/* time statistics
SQL Server Execution Times:
   CPU time = 0 ms,  elapsed time = 1181 ms.
*/

--�޸ĺ��д��
select count(*)
FROM  ACT.dbo.Newegg_InvoiceMaster b WITH(NOLOCK)  
    INNER JOIN Newsql.Fedex.dbo.v_fa_somaster a WITH(NOLOCK) 
		ON  b.InvoiceNumber = a.InvoiceNumber                         
    WHERE (b.InvoiceDate >= '2008-8-1' AND b.InvoiceDate < getdate())                        
    AND (b.FedexShippingCharge IS NULL OR b.FedexShippingCharge = 0) 

/* time statistics
10����û�г�������
*/


/*==========================The third part:Commonly Errors=====================*/

/*
1.��ͼ�� ON �����й��˲����������ļ�¼:
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
--�����д��
SELECT A.ID,A.NAME, B.ID,B.name
FROM dbo.t1 A WITH (NOLOCK)
 LEFT JOIN dbo.t2 B
  ON A.ID = B.ID 
   AND A.name = 'a'
   AND B.name = 'c'

--��ȷ��д��
SELECT A.ID,A.NAME, B.ID,B.name
FROM dbo.t1 A WITH (NOLOCK)
 LEFT JOIN dbo.t2 B WITH (NOLOCK)
  ON A.ID = B.ID AND B.name = 'c'
WHERE A.name = 'a'


--�Ƚ���������д���Ĳ���
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
4.ʹ��LEFT JOIN���WHERE�����У��ж��ұ߱�ؼ�ֵ�Ƿ�ΪNULL����ѯ��������߱���ֵļ�¼
*/

SELECT A.*
FROM dbo.T1 AS A WITH (NOLOCK)
	LEFT JOIN dbo.T2 AS B WITH (NOLOCK)
	ON A.id=B.id
WHERE B.id IS NULL


--�����޸ģ�
SELECT *
FROM dbo.T1 AS A WITH (NOLOCK)
WHERE NOT EXISTS(SELECT TOP 1 1
			FROM dbo.T2 AS B WITH (NOLOCK)
			WHERE B.id=A.id
			)
--����
SELECT *
FROM dbo.T1 AS A WITH (NOLOCK)
WHERE ID NOT IN (SELECT ID
			FROM dbo.T2 AS B WITH (NOLOCK)
			)
-------------------------------------------------------------------
/*----------------------------------
--BYDBA 1.����ʹ��SET XACT_ABORT ON �Զ��ع����񣬾���ʹ��TRY...CATCH���

*/

--������
SET XACT_ABORT ON

BEGIN
	BEGIN TRAN
		UPDATE dbo.Users
		SET Name='Test'
		WHERE ID=123456
	COMMIT
END

--��������
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
���Ǿ����ܱ�֤�����С����Ӧ��������ѭ��ʹ��һ�����񣬶��ǲ�����ÿһ������ʹ��ѭ���ķ�ʽ

������һ������ȷ��demo
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
--BYDBA 3.���������ʱ����Ϊ���ٻ��ͬһ�ű��ѯ����
�������β�ѯ�Ĵ󲿷�������ͬ��
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

--��������:
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
--BYDBA 3.INNER Join���Ӳ���,�뽫ɸѡ�����������������룬
--ɸѡ�����ŵ�Where�־��С�
---------------------------------------------------*/
SELECT A.edate
		,B.vendno
FROM [SCM].[dbo].[arinvt01] AS A WITH (NOLOCK)
	INNER JOIN [SCM].[dbo].[potran01] AS B WITH (NOLOCK)
	ON A.Item=B.Item AND B.purno='105336'

--�޸�Ϊ��
SELECT A.edate
		,B.vendno
FROM [SCM].[dbo].[arinvt01] AS A WITH (NOLOCK)
	INNER JOIN [SCM].[dbo].[potran01] AS B WITH (NOLOCK)
	ON A.Item=B.Item
WHERE B.purno='105336'
-------------------------------------------------------------------
/*------------------------------------------------------------------
--BYDBA 1.ʹ��COUNT(*)�жϴ������
------------------------------------------------------------------*/

IF (SELECT COUNT(*) 
	FROM [SCM].[dbo].[VendorReturnPolicy] WITH (NOLOCK) 
	WHERE VendorNumber='20001') >0      
BEGIN      
    SELECT VendorCreditDays,VRPNumber
	FROM dbo.VendorReturnPolicy WITH (NOLOCK) 
	WHERE VRPNumber = 3279        
END

--�޸�Ϊ:
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
1.ʹ�� ISNULL(Col, 0) ���� CASE WHEN Col IS NULL THEN 0 ELSE Col END
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
2.ͨ������ķ��������� SELECT �����ʹ�� DISTINCT
*/
SELECT DISTINCT
		A.au_fname
		,A.au_lname
FROM dbo.authors AS A WITH (NOLOCK) 
	INNER JOIN dbo.titleAuthor AS T WITH (NOLOCK)
ON T.au_id = A.au_id

--����DISTINCT��д��
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
3.ͨ������ķ���������Like�־䣬��sql��� WHERE �����ʹ�� col LIKE '%a'
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

--�����޸�Ϊ��
--   CPU time = 983 ms,  elapsed time = 267 ms.
SELECT COUNT(1)--45991
FROM [SCM].[dbo].[potran01] AS B WITH (NOLOCK)
WHERE RIGHT(RTRIM(B.purno),1)='5'

-------------------------------------------------------------------
/*
5.�������У� ��Ҫ���������������
*/
SELECT *
FROM dbo.T1 WITH (NOLOCK)
WHERE 1=1
-------------------------------------------------------------------
/*
6.ɾ����������ʱ������ʹ�� TRUNCATE TABLE
*/
DELETE FROM dbo.T1

--����
TRUNCATE TABLE dbo.T1
-------------------------------------------------------------------
--Question:
/*
��ʲô����£�ֻ��ʹ��DELETE������ʹ��TRUNCATE�أ�
*/


/*
--BYDBA 3.ntext, text, and image �������Ͳ�������ʹ�ã�����nvarchar(max), varchar(max), and varbinary(max),xml ���档
��ʱ����ʱ�������Ҳ������ʹ��

�� Microsoft SQL Server ��δ���汾�н�ɾ�� ntext��text �� image �������͡�
����� nvarchar(max)��varchar(max) �� varbinary(max)��
*/

/*--BYDBA 3.��CREAET Function��ʱ�򣬽�����ʾ������ṹ��
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

--�޸�Ϊ��
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
--BYDBA 1.��ȷ���Ƿ���Ҫ֧�ֶ�����ԣ��������UNICODE������������͡�

--IN S7DBM01
create table dbo.TUnicoude
(id int identity(1,1)
,Name1 varchar(10)
,Name2 Nvarchar(10)
)

insert into dbo.TUnicoude
select 'dfsdsd',N'abcd'
union all
select N'������','������'
union all
select '������',N'������'

select * from dbo.TUnicoude with (nolock)

drop table dbo.TUnicoude


---------------------------------------------�ϲ��ַ���
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

--����һ��
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

--������
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

--------------------------------------�ָ��ַ���
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


---================================================�ܽ��һЩר��================================================

------------------------------------�α�ר��----------------------------------------------
/*
14.�����α�ʱ����������ر���Ҫ��ʹ��LOCAL�ؼ���ʽ�Ľ��α궨��Ϊ�ֲ��α꣬
��������ʹ��ȫ�֣�GLOBAL���������ݿ��Ĭ����Ϊ���αꣻ
û��������Ҫ�Ļ�������ʹ�� FORWARD_ONLY READONLY �α�,ͬʱ����ʹ�þ�̬�α�  STATIC��
����������ݵĴ������ʹ���α�"
*/
DECLARE cur_MyCursor CURSOR LOCAL STATIC READ_ONLY FORWARD_ONLY
FOR
SELECT TOP 10 QUOTENAME(SCHEMA_NAME(schema_id),'[]')+'.'+QUOTENAME(name,'[]')
FROM sys.tables WITH (NOLOCK)

/*---------------BYDBA 3.������ʹ���α�,��ʹ�������ķ�����----------------
1.ƴ���ַ���
2.update/delete/insert ��¼
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

--����SELECTʵ�֡�
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


------------------------------------���ݻ���ר��----------------------------------------------
/*
15.������ʱ����ʱ��ʹ����������ʱ��#����##��ͷ���ı����⽫��ʱ���浽��ʽ����
*/

--���������

SET NOCOUNT ON
DECLARE @string NVARCHAR(2000)
SET @string=''

SELECT TOP 10 QUOTENAME(SCHEMA_NAME(schema_id),'[]')+'.'+QUOTENAME(name,'[]')+',' AS Name
INTO dbo.TableName--��ʽ�����ڻ��������
FROM sys.tables WITH (NOLOCK)

SELECT @string=ISNULL(@string,'')+Name
FROM dbo.TableName WITH (NOLOCK)
PRINT @string
DROP TABLE dbo.TableName

----��ȷ������1.
SET NOCOUNT ON
DECLARE @string NVARCHAR(2000)
SET @string=''

IF OBJECT_ID('tempdb.dbo.#temp','U') IS NOT NULL
BEGIN
	DROP TABLE #temp
END


SELECT TOP 10 QUOTENAME(SCHEMA_NAME(schema_id),'[]')+'.'+QUOTENAME(name,'[]')+',' AS Name
INTO #temp--�ֲ���ʱ��������
FROM sys.tables WITH (NOLOCK)

SELECT @string=ISNULL(@string,'')+Name
FROM #temp
PRINT @string
IF OBJECT_ID('tempdb.dbo.#temp','U') IS NOT NULL
BEGIN
	DROP TABLE #temp
END

----��ȷ������2.
SET NOCOUNT ON
DECLARE @string NVARCHAR(2000)
SET @string=''

IF OBJECT_ID('tempdb.dbo.##temp','U') IS NOT NULL
BEGIN
	DROP TABLE ##temp
END


SELECT TOP 10 QUOTENAME(SCHEMA_NAME(schema_id),'[]')+'.'+QUOTENAME(name,'[]')+',' AS Name
INTO ##temp--ȫ����ʱ��������
FROM sys.tables WITH (NOLOCK)

SELECT @string=ISNULL(@string,'')+Name
FROM ##temp
PRINT @string
IF OBJECT_ID('tempdb.dbo.##temp','U') IS NOT NULL
BEGIN
	DROP TABLE ##temp
END

----��ȷ������3.
SET NOCOUNT ON
DECLARE @string NVARCHAR(2000)
SET @string=''

DECLARE @Name TABLE
(
	Id INT IDENTITY(1,1)
	,TableName SYSNAME
)

--�������������
INSERT INTO @Name(TableName)
SELECT TOP 10 QUOTENAME(SCHEMA_NAME(schema_id),'[]')+'.'+QUOTENAME(name,'[]')+','
FROM sys.tables WITH (NOLOCK)

SELECT @string=ISNULL(@string,'')+TableName
FROM @Name
PRINT @string
--�ڴ˴�������ʱ�����������ֲ��������ȫ�ֱ����������
/*
��ʱ��������
��ͬ�㣺����SQL�������ʱ�Ļ�������
��ͬ�㣺1.��ʱ�����ݴ洢������Ĵ�����(���ݱ�����Tempdb��)��������洢���ڴ��У�
		2.����1�����Ի�����������ʱ�������Ч�ʱ���ʱ��ߣ�
		3.������޷�����INDEX���޷��޸Ľṹ���޷��������򣬵�����ʱ��պ��෴��
		4.����3�����Ի����������ʱ��ʹ����ʱ���INDEXЧ�ʱ�����ߣ�
		5.��������������Ӱ�죬��ʱ����������Ӱ�졣

�ֲ���ʱ���ȫ����ʱ��
���������ơ��ɼ����Լ���������������
������ʱ��������Ե������ַ��� (#) ��ͷ�����ǽ��Ե�ǰ���û������ǿɼ��ģ����û��� SQL Server ʵ���Ͽ�����ʱ��ɾ����
ȫ����ʱ����������������ַ��� (##) ��ͷ����������κ��û����ǿɼ��ģ����������øñ���û��� SQL Server �Ͽ�����ʱ��ɾ����
*/





------------------------------------��̬SQLר��----------------------------------------------
/*
16.��̬T-SQL��������У�����漰������������ʹ��sp_executesql��ͨ���������ݽ��д���
����ʹ��EXECӲƴSQL���洢���̻����в��������δʹ�õĲ��������

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

--�Ƽ�д��
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


---------------------Demo3:��̬SQL�е�like�Ӿ���
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

-----�����ǲ��Դ���
DECLARE @sql NVARCHAR(max)
		,@Like NVARCHAR(10)

SET @Like=N'76'

--��ǰ����ƴ���Ĵ���ʽ(�ᵼ��SQL�ر���ִ�д���Ӱ������)
SET @sql=N'SELECT * FROM dbo.TestLike WITH (NOLOCK)
			WHERE Name like ''%'+@Like+'%'''

EXEC sp_executesql @sql

--�µĴ���ʽ���ѱ����ŵ�ִ�д��С�(����SQL�ر���ִ�д�)
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

---------------------Demo4:����̬SQL�Ĳ����ŵ�ִ�д���

/*
����֪����SQL�����ִ��ǰ���Ƚ������벢ͨ����ѯ�Ż���������Ż����Ӷ��õ��Ż����ִ�мƻ���Ȼ����ִ�мƻ���ִ�С�
�����������ơ������ǲ����� ͬ��SQL��䣬SQL Server��������ִ�мƻ���
�����ڲ�ͬ��SQL��䣬SQL Server�������ظ�ʹ����ǰ��ִ�мƻ���������Ҫ���±����һ���µ�ִ�мƻ���
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


----------------�Ƚ�EXEC��sp_executesql������


------sp_executesql �������
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


------sp_executesql��ȫ��Ч��

--DBCC FREEPROCCACHE 
declare @sql nvarchar(max)
       ,@type sysname

SET @sql=N'
select top 5 * 
from sys.sysobjects with (Nolock)
WHERE 1=1'

--set @type=N'u'
SET @type=N'u'''+' OR 1=1;EXEC master.dbo.xp_create_subdir ''C:\temp''--'----����ȫ�Ĵ���

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
1.����Щ����£�����ʹ��sp_executesql���θ�ʽ��
2.Demo�еı���@sql����������ʹ��VARCHAR(MAX)������
*/


------------------------------------����Distinctר��----------------------------------------------
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
--�ҳ��Ѿ������������

--�����������Ƿ��ظ��Ĳ�ѯ�벻Ҫʹ��Distinct
SELECT B.Author
FROM dbo.Test_Artiles AS A WITH (NOLOCK)
	INNER JOIN dbo.Test_Author AS B WITH (NOLOCK)
	ON A.AuthorID=B.ID

--IN�־��еĲ�ѯ�벻Ҫʹ��Distinct
--�����д��
SELECT B.Author
FROM dbo.Test_Author AS B WITH (NOLOCK)
WHERE B.ID IN(
				SELECT DISTINCT AuthorID
				FROM dbo.Test_Artiles AS A WITH (NOLOCK)
			 )

--��ȷ��д��
SELECT B.Author
FROM dbo.Test_Author AS B WITH (NOLOCK)
WHERE B.ID IN(
				SELECT  AuthorID
				FROM dbo.Test_Artiles AS A WITH (NOLOCK)
			 )

--�벻Ҫ��Distinct��UNION����UNION ALL����ʹ��

--�����д��һ
SELECT DISTINCT AuthorID,Author
FROM
	(
		SELECT 1 AS AuthorID,'Jim' AS Author
		UNION ALL	
		SELECT 1,'Jim'
	) AS A

--�����д����
SELECT DISTINCT AuthorID,Author
FROM
	(
		SELECT 1 AS AuthorID,'Jim' AS Author
		UNION	
		SELECT 1,'Jim'
	) AS A

-- ��ȷ��д��
SELECT AuthorID,Author
FROM
	(
		SELECT 1 AS AuthorID,'Jim' AS Author
		UNION	
		SELECT 1,'Jim'
	) AS A

--�벻Ҫ��Distinct��Group Byһ��ʹ��

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

--��ĳЩ����£�ʹ���Ӳ�ѯ������Distinct
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

--��û�оۺϺ���������£�������ʹ��Distinct������Group By
--����ʹ��
SELECT DISTINCT B.Author
FROM dbo.Test_Artiles AS A WITH (NOLOCK)
	INNER JOIN dbo.Test_Author AS B WITH (NOLOCK)
	ON A.AuthorID=B.ID

--���Ƽ�
SELECT B.Author
FROM dbo.Test_Artiles AS A WITH (NOLOCK)
	INNER JOIN dbo.Test_Author AS B WITH (NOLOCK)
	ON A.AuthorID=B.ID
GROUP BY B.Author
--�ں��оۺϺ���������£���ʹ��Group BY������Distinct

SELECT B.Author,COUNT(AuthorID) AS ArtilesCount
FROM dbo.Test_Artiles AS A WITH (NOLOCK)
	INNER JOIN dbo.Test_Author AS B WITH (NOLOCK)
	ON A.AuthorID=B.ID
GROUP BY B.Author
ORDER BY ArtilesCount DESC

-----------============================== XMLר�� ========================================

--BYDBA 1.�漰��xml����ʱ��ʹ��sql server 2005 ����xml�Ĵ���ʽ

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

--SQLServer 2000��XML�Ĵ���ʽ.  --����ʹ��

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


--�Ƽ�����:
SELECT T.c.value('(./Order/@OrderID)[1]','INT')					AS OrderID
		,T.c.value('(./@CustomerID)[1]','VARCHAR(10)')			AS CustomerID
		,T.c.value('(./Order/@OrderDate)[1]','DATETIME')		AS OrderDate
		,T.c.value('(./Order/OrderDetail/@ProductID)[1]','INT') AS ProdID
		,T.c.value('(./Order/OrderDetail/@Quantity)[1]','INT')	AS Qty
FROM @doc.nodes('/ROOT/Customer') T(c)
-----------======================================================================

--BYDBA 1.XML�������ͱ����Ǻ�UNICODE���͵������໥ת����

--IN S7DBM01
DECLARE 
    @x1 xml,
    @s1 varchar(max)
    
SET @x1 = CONVERT(xml,N'<root>����</root>')
SET @s1 = CONVERT(varchar(max),@x1)
GO

--������Ĵ���,�򲻻���ִ���:

DECLARE 
    @x1 nvarchar(max),
    @s1 nvarchar(max)
    
SET @x1 = N'<root>����</root>'
SET @s1 = CONVERT(nvarchar(max),@x1)
GO

DECLARE 
    @x1 xml,
    @s1 nvarchar(max)  --���ﶨ��Ϊnvarchar
    
SET @x1 = CONVERT(xml,N'<root>����</root>')
SET @s1 =  CONVERT(nvarchar(max),@x1)
GO
-----------======================================================================
--BYDBA 3.����XML�����б�����ֵ
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
--д��һ
SELECT TOP 1  
	@NameSpace =T.c.value('(Namespace)[1]', 'INT')  
	,@GuidID = T.c.value('(OriginalGUID)[1]', 'INT')  
	,@UerID = T.c.value('(UerID)[1]', 'INT')  
 FROM @Message.nodes('/Publish/Node/MessageHead') T (c) 

select @NameSpace,@GuidID,@UerID

--д����
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

--д����(���ŵ�д��)
--BYDBA �޸ĺ��д�������㲻ͬ��1.����Ҫȥ����nodes 2.������text()������

DECLARE @UerID_bak INT
SELECT
	@NameSpace=@Message.value('(/Publish/Node/MessageHead/Namespace/text())[1]', 'INT')  
	,@GuidID= @Message.value('(/Publish/Node/MessageHead/OriginalGUID/text())[1]', 'INT')
	,@UerID= @Message.value('(/Publish/Node/MessageHead/UerID/text())[1]', 'INT')
	,@UerID_bak= ISNULL(@Message.value('(/Publish/Node/MessageHead/UerID/text())[1]', 'INT'),0)

select @NameSpace,@GuidID,@UerID,@UerID_bak

-- �Ƚ� д��һ,��
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

--BYDBA 1.����ظ�������ͬ��XML namespace
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

--BYDBA �޸ĺ��д����
;WITH XMLNAMESPACES (DEFAULT 'http://soa.newegg.com/SOA/USA/InfrastructureService/V10/PubSubService')
SELECT @SessionID=@XML.value('(/Publish/Node/SessionID/text())[1]','CHAR(15)')
		,@OrderNumber=@XML.value('(/Publish/Node/OrderNumber/text())[1]','int')
		,@CustomerNumber=@XML.value('(/Publish/Node/CustomerNumber/text())[1]','int')

SELECT @SessionID,@OrderNumber,@CustomerNumber

---�������ռ�ȡֵ
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

--BYDBA 3.��ָ��XML�ľ���·������'//'�޸�Ϊ'/'
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

--ʹ��namspace�����·��
;WITH XMLNAMESPACES (DEFAULT 'http://soa.newegg.com/SOA/USA/InfrastructureService/V10/PubSubService')
SELECT @SessionID=@XML.value('(//SessionID/text())[1]','CHAR(15)')
		,@OrderNumber=@XML.value('(//OrderNumber/text())[1]','int')
		,@CustomerNumber=@XML.value('(//CustomerNumber/text())[1]','int')

SELECT @SessionID,@OrderNumber,@CustomerNumber;

--��������namespace�µĽڵ�
SELECT @SessionID=@XML.value('(/*:Publish/*:UserData/*:Node/*:SessionID/text())[1]','CHAR(15)')
		,@OrderNumber=@XML.value('(/*:Publish/*:UserData/*:Node/*:OrderNumber/text())[1]','int')
		,@CustomerNumber=@XML.value('(/*:Publish/*:UserData/*:Node/*:CustomerNumber/text())[1]','int')

SELECT @SessionID,@OrderNumber,@CustomerNumber

--BYDBA �޸ĺ��д����
;WITH XMLNAMESPACES (DEFAULT 'http://soa.newegg.com/SOA/USA/InfrastructureService/V10/PubSubService')
SELECT @SessionID=@XML.value('(/Publish/UserData/Node/SessionID/text())[1]','CHAR(15)')
		,@OrderNumber=@XML.value('(/Publish/UserData/Node/OrderNumber/text())[1]','int')
		,@CustomerNumber=@XML.value('(/Publish/UserData/Node/CustomerNumber/text())[1]','int')

SELECT @SessionID,@OrderNumber,@CustomerNumber
-----------======================================================================

--ʹ��exist()��value()ʱ�����ܵ�Ӱ��
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

--ʹ��exist
;WITH XMLNAMESPACES(DEFAULT 'http://soa.newegg.com/SOA/USA/InfrastructureService/V10/PubSubService')
SELECT @XML.value('(/Publish/UserData/Node/UserData/text())[1]','VARCHAR(10)')
		,@XML.value('(/Publish/UserData/Node/SessionID/text())[1]','VARCHAR(10)')
		,@XML.value('(/Publish/UserData/Node/OrderNumber/text())[1]','INT')
		,@XML.value('(/Publish/UserData/Node/CustomerNumber/text())[1]','INT')
WHERE @XML.exist('/Publish/UserData/Node/OrderNumber[text()="102365"]')=1

--ʹ����ͨ����(value)
;WITH XMLNAMESPACES(DEFAULT 'http://soa.newegg.com/SOA/USA/InfrastructureService/V10/PubSubService')
SELECT @XML.value('(/Publish/UserData/Node/UserData/text())[1]','VARCHAR(10)')
		,@XML.value('(/Publish/UserData/Node/SessionID/text())[1]','VARCHAR(10)')
		,@XML.value('(/Publish/UserData/Node/OrderNumber/text())[1]','INT')
		,@XML.value('(/Publish/UserData/Node/CustomerNumber/text())[1]','INT')
WHERE @XML.value('(/Publish/UserData/Node/OrderNumber/text())[1]','INT')=102365

-----------======================================================================

--ʹ��local-name()�����ܲ�����Ӱ��
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

--�����κ������ռ��Ӧ�ڵ�ֵ�����Ƽ���д��
SELECT @XML.value('(/*[local-name()="Publish"]/*[local-name()="UserData"]/*[local-name()="Node"]/*[local-name()="UserData"]/text())[1]','VARCHAR(10)')
		,@XML.value('(/*[local-name()="Publish"]/*[local-name()="UserData"]/*[local-name()="Node"]/*[local-name()="SessionID"])[1]','VARCHAR(10)')
		,@XML.value('(/*[local-name()="Publish"]/*[local-name()="UserData"]/*[local-name()="Node"]/*[local-name()="OrderNumber"])[1]','INT')
		,@XML.value('(/*[local-name()="Publish"]/*[local-name()="UserData"]/*[local-name()="Node"]/*[local-name()="CustomerNumber"])[1]','INT')

--�����ض��������ռ䣬�Ƽ���д��
;WITH XMLNAMESPACES(DEFAULT 'http://soa.newegg.com/SOA/USA/InfrastructureService/V10/PubSubService')
SELECT	@XML.value('(/Publish/UserData/Node/UserData/text())[1]','VARCHAR(10)')
		,@XML.value('(/Publish/UserData/Node/SessionID/text())[1]','VARCHAR(10)')
		,@XML.value('(/Publish/UserData/Node/OrderNumber/text())[1]','INT')
		,@XML.value('(/Publish/UserData/Node/CustomerNumber/text())[1]','INT') 

-----------======================================================================

--�޸�XML�Ľڵ�
DECLARE @POData xml
		, @InvoiceNumber int 
set @InvoiceNumber=45386473
--������δ����ǰ�ԭ����XML�����VendorInvoiceNumber�޸�Ϊ��<VendorInvoiceNumber xmlns="">45386473</VendorInvoiceNumber>
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

--��ɾ��
SET @POData.modify('          
  delete /*:Publish/*:Node/*:POV10/*:Body/*:PO/*:VendorInvoiceNumber          
')    
      
--NameSpace��������
--SET @POData.modify('          
--  insert <VendorInvoiceNumber>{sql:variable("@InvoiceNumber")}</VendorInvoiceNumber>          
--  into (/*:Publish/*:Node/*:POV10/*:Body/*:PO)[1]          
--')    
--
--select @POData

--��ȷ�����
SET @POData.modify('declare default element namespace "http://soa.newegg.com/SOA/USA/InfrastructureService/V10/PubSubService";        
  insert <VendorInvoiceNumber>{sql:variable("@InvoiceNumber")}</VendorInvoiceNumber>          
  into (/Publish/Node/POV10/Body/PO)[1]          
')    

select @POData

-----------======================================================================

--XML�������ںϲ��ַ���
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

--����һ��
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

--������
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

-----XML�������ڷָ��ַ���
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
--XML��������Ϊ���������

DECLARE @xml xml 
SET @xml = 
--BYDBA 1.XML������ֵ�ǣ������Ǽ�N
N'
<Publish xmlns="http://soa.newegg.com/SOA/USA/InfrastructureService/V10/PubSubService">
  <Node>
    <ComReasonTreeMessage>
      <Body>        
        <ReasonDescription>����XML��������Ϊ���������</ReasonDescription>
      </Body>
    </ComReasonTreeMessage>
  </Node>
</Publish>'

DECLARE @ReasonDescription1 VARCHAR(100)

;WITH XMLNAMESPACES (DEFAULT 'http://soa.newegg.com/SOA/USA/InfrastructureService/V10/PubSubService')
SELECT @ReasonDescription1=@xml.value('(Publish/Node/ComReasonTreeMessage/Body/ReasonDescription/text())[1]','VARCHAR(100)')

DECLARE @ReasonDescription NVARCHAR(100)--BYDBA 1.�˴����붨��ΪUNICODE�������������
;WITH XMLNAMESPACES (DEFAULT 'http://soa.newegg.com/SOA/USA/InfrastructureService/V10/PubSubService')
SELECT @ReasonDescription=@xml.value('(Publish/Node/ComReasonTreeMessage/Body/ReasonDescription/text())[1]','NVARCHAR(100)')--BYDBA 1.�˴�����ΪUNICODE�������������

select @ReasonDescription1,@ReasonDescription

---ͳ��XML�ڵ����
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

--���Ƽ���д��
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


--���Ƽ�������
;WITH XMLNAMESPACES (DEFAULT 'http://soa.newegg.com/SOA/USA/InfrastructureService/V10/PubSubService')
SELECT COUNT(*)
FROM (
SELECT  T.C.query('.') AS Node
FROM @xml.nodes('/Publish/Node') AS T(C)
) AS A

--�Ƽ���д��
;WITH XMLNAMESPACES (DEFAULT 'http://soa.newegg.com/SOA/USA/InfrastructureService/V10/PubSubService')
SELECT @xml.value('count(/Publish/Node)','INT')

---------------------------------------===========================���XML
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


-------------------------���Ƽ���д��

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
-------------------------�������д��
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
-------------------------�����д��
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


--------------------------ѭ������XML����������
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

-----------------------------------����Ĵ���ʽ

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

---------------------------------------����xml�ڵ����������
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
,'xmlns="http://soa.newegg.com/SOA/CN/InfrastructureService/V10/NeweggCNPubSubService"','') as xml).query('/Publish/Node/Root')  --SSBRouter��ʽ

--��ȷ��д��
select @messageBody.query('declare default element namespace "http://soa.newegg.com/SOA/CN/InfrastructureService/V10/NeweggCNPubSubService" 
;/Publish/Node/Root')

------------------------ƴ��XML
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
--BYCherish:ƴ���ַ�����д��
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
--BYCherish���Ƽ���д��
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