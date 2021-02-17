--since CREATE SCHEMA must be in it's own batch, this is how to create it if it doesn't already exist
IF SCHEMA_ID('Tools') IS NULL
	EXEC ('CREATE SCHEMA Tools;')
GO
--TODO: Add security to this schema after you create.

CREATE OR ALTER FUNCTION Tools.String$EscapeString
(
	@inputString nvarchar(4000), --would work on varchar(max) too
	@character nchar(1) = N'''', --if you needed that
	@surroundOutputFlag bit = 1
) 
RETURNS nvarchar(4000)
AS
  BEGIN
	RETURN (CASE WHEN @surroundOutputFlag = 1 THEN @character END
	       +REPLACE(@inputString,@character,@character+@character)
	       +CASE WHEN @surroundOutputFlag = 1 THEN @character END)
  END;
GO

 CREATE OR ALTER PROCEDURE Tools.Table$ListExtendedProperties
	@schema_name_like sysname = '%',
	@table_name_like sysname = '%',
	@property_name_like sysname = '%'
------------------------------------------------------------------------
-- List the extended property on tables, based on a set of like expressions
--
-- 2020 Louis Davidson – drsql@hotmail.com – drsql.org 
------------------------------------------------------------------------

WITH EXECUTE AS OWNER --need extra rights to view extended properties
AS
 BEGIN
	SELECT schemas.name AS schema_name,  tables.name AS table_name, 
	       extended_properties.name AS property_name, 
		   extended_properties.value AS property_value
	FROM   sys.extended_properties 
	           JOIN sys.tables
				JOIN sys.schemas	
					ON tables.schema_id = schemas.schema_id
			ON tables.object_id = extended_properties.major_id
	WHERE  extended_properties.class_desc = 'OBJECT_OR_COLUMN'
	  AND  extended_properties.minor_id = 0
	  AND  schemas.name LIKE @schema_name_like
	  AND  tables.name LIKE @table_name_like
	  AND  extended_properties.name LIKE @property_name_like
	ORDER BY schema_name, table_name, property_name;
  END
GO
CREATE OR ALTER FUNCTION Tools.SystemSecurityName$Get
(
     @AllowSessionContext bit = 1,
     @IgnoreImpersonation bit = 0
)
------------------------------------------------------------------------
-- Get the user’s security context, using SESSION_CONTEXT, SUSER_SNAME,
-- or ORIGINAL_LOGIN
--
-- 2020 Louis Davidson – drsql@hotmail.com – drsql.org 
------------------------------------------------------------------------
RETURNS sysname
AS
 BEGIN
    RETURN (
     CASE WHEN @AllowSessionContext = 1 
                 AND SESSION_CONTEXT(N'ApplicationUserName') IS NOT NULL
              THEN CAST(SESSION_CONTEXT(N'ApplicationUserName') AS sysname)
          WHEN @IgnoreImpersonation = 1
              THEN SUSER_SNAME()
          ELSE ORIGINAL_LOGIN() END)
 END;
GO

----------------------------------------------------------------------------------------------------------
--********************************************************************************************************
--Numbers table
--********************************************************************************************************
----------------------------------------------------------------------------------------------------------

IF OBJECT_ID('Tools.Number') IS NULL
	CREATE TABLE Tools.Number
	(
		I   int CONSTRAINT PKNumber PRIMARY KEY
	);

--Load it with integers from 0 to 999999:
;WITH digits (I) AS (--set up a set of numbers from 0-9
        SELECT I
        FROM   (VALUES (0),(1),(2),(3),(4),(5),(6),(7),(8),(9)) AS digits (I))
--builds a table from 0 to 999999
,Integers (I) AS (
       --since you have every combinations of digits, This math turns it 
       --into numbers since every combination of digits is present
        SELECT D1.I + (10*D2.I) + (100*D3.I) + (1000*D4.I) + (10000*D5.I)
               + (100000*D6.I)
        --gives us combinations of every digit
        FROM digits AS D1 CROSS JOIN digits AS D2 CROSS JOIN digits AS D3
                CROSS JOIN digits AS D4 CROSS JOIN digits AS D5
                CROSS JOIN digits AS D6 )
INSERT INTO Tools.Number(I)
SELECT I
FROM   Integers
WHERE  I NOT IN (SELECT I FROM TOols.Number);


----------------------------------------------------------------------------------------------------------
--********************************************************************************************************
--Calendar Table
--********************************************************************************************************
----------------------------------------------------------------------------------------------------------

IF OBJECT_ID('Tools.Calendar') IS NULL
	CREATE TABLE Tools.Calendar
	(
			DateValue date NOT NULL CONSTRAINT PKTools_Calendar PRIMARY KEY,
			DayName varchar(10) NOT NULL,
			MonthName varchar(10) NOT NULL,
			Year varchar(60) NOT NULL,
			Day tinyint NOT NULL,
			DayOfTheYear smallint NOT NULL,
			Month smallint NOT NULL,
			Quarter tinyint NOT NULL
	);

--load up to the next 2 years
DECLARE @enddate date = CAST(YEAR(GETDATE() + 2) as char(4)) + '0101';
WITH Dates (NewDateValue) AS (
        --pick some base date for your calendar, it doesn’t really matter
        SELECT DATEADD(day,I,'19000101') AS NewDateValue
        FROM Tools.Number
)
INSERT Tools.Calendar
        (DateValue,DayName
        ,MonthName,Year,Day
        ,DayOfTheYear,Month,Quarter
)
SELECT
        Dates.NewDateValue as DateValue,
        DATENAME(dw,Dates.NewDateValue) As DayName,
        DATENAME(mm,Dates.NewDateValue) AS MonthName,
        DATENAME(yy,Dates.NewDateValue) AS Year,
        DATEPART(day,Dates.NewDateValue) AS Day,
        DATEPART(dy,Dates.NewDateValue) AS DayOfTheYear,
        DATEPART(m,Dates.NewDateValue) AS Month,
        DATEPART(qq,Dates.NewDateValue) AS Quarter

FROM    Dates
WHERE   Dates.NewDateValue BETWEEN '20000101' AND @enddate
  AND   Dates.NewDateValue NOT IN (SELECT DateValue FROM Tools.Calendar)
ORDER   BY DateValue;

GO

CREATE OR ALTER FUNCTION Tools.String$SplitPart
(
    @inputValue nvarchar(4000),
    @delimiter  nchar(1) = ',',  
    @position   int = 1
)
------------------------------------------------------------------------
-- Helps to normalize a delimited string by fetching one value from the
-- list. (note, can’t use STRING_SPLIT because return order not guaranteed)
--
-- 2020 Louis Davidson – drsql@hotmail.com – drsql.org 
------------------------------------------------------------------------
RETURNS nvarchar(4000)
WITH SCHEMABINDING, EXECUTE AS CALLER AS
BEGIN
       DECLARE @start int, @end int
       --add commas to end and start
       SET @inputValue = @delimiter + @inputValue + @delimiter;

       WITH BaseRows AS (
            SELECT Number.I, 
                   ROW_NUMBER() OVER (ORDER BY Number.I) AS StartPosition, 
                   ROW_NUMBER() OVER (ORDER BY Number.I) - 1 AS EndPosition
            FROM   Tools.Number
            WHERE  Number.I <= LEN(@inputValue)
             AND  SUBSTRING(@inputValue,Number.I,1) = @delimiter
       )                   --+1 to deal with commas
       SELECT @start = (SELECT BaseRows.I + 1 FROM BaseRows 
                        WHERE BaseRows.StartPosition = @Position),
              @end = (  SELECT BaseRows.I FROM BaseRows 
                        WHERE BaseRows.EndPosition = @Position)

       RETURN SUBSTRING(@inputValue,@start,@end - @start)
 END;
 GO

 CREATE OR ALTER FUNCTION Tools.Number$Translate3DigitsToWords
(
	@NumberToTranslate numeric(3,0)
)
RETURNS varchar(100)
AS
BEGIN
	IF @NumberToTranslate = 0
		RETURN null

	DECLARE @NumberToTranslateText varchar(3), @output nvarchar(500)
	SET @NumberToTranslateText = RIGHT(REPLICATE('0',3) + CAST(@numberToTranslate AS nvarchar(3)),3)

	DECLARE @FirstDigit table (Value char(1), Word varchar(10))
	INSERT INTO @FirstDigit(Value, Word)
	values('0',''),('1','One'),
	('2','Two'),('3','Three'),('4','Four'),('5','Five'),
	('6','Six'),('7','Seven'),('8','Eight'),('9','Nine')

	DECLARE @Teens table (Value char(2), Word varchar(10))
	INSERT INTO @Teens(Value, Word)
	VALUES ('10','Ten'),('11','Eleven'),('12','Twelve'),('13','Thirteen'),
	('14','Forteen'),('15','Fifteen'),('16','Sixteen'),
	('17','Seventeen'),('18','Eighteen'),('19','Nineteen')


	DECLARE @Tens table (Value char(1), Word varchar(10))
	INSERT INTO @Tens(Value, Word)
	VALUES('0',''),('2','Twenty'),('3','Thirty'),
	('4','Forty'),('5','Fifty'),('6','Sixty'),
	('7','Seventy'),('8','Eighty'),('9','Ninety')

	SELECT @Output = Word
	FROM   @Teens
	WHERE  Value = SUBSTRING(@NumberToTranslateText,2,2)

	IF @output IS NULL
		SELECT @output = word
		FROM @FirstDigit
		WHERE  Value = SUBSTRING(@NumberToTranslateText,3,1) 

	SELECT @output = CONCAT(Word, IIF(@output = '' ,'','-'), + @output )
	FROM  @Tens
	WHERE  Value = SUBSTRING(@NumberToTranslateText,2,1)
	  AND  SUBSTRING(@NumberToTranslateText,2,1) <>  0

	SELECT @output = CONCAT(Word, '-Hundred ', IIF(@output = '','','and '), @output)
	FROM  @FirstDigit
	WHERE  Value = SUBSTRING(@NumberToTranslateText,1,1)
	  AND  SUBSTRING(@NumberToTranslateText,1,1) <> '0'

	RETURN TRIM(@output)
 END
 GO

 CREATE OR ALTER FUNCTION Tools.Number$TranslateToWords
(
	@NumberToTranslate numeric(15,0)
)
RETURNS varchar(100)
AS
BEGIN

	DECLARE @NumberToTranslateText varchar(15), @output nvarchar(100)
	SET @NumberToTranslateText = RIGHT(REPLICATE('0',15)  + CAST(@numberToTranslate AS nvarchar(15)),15)

	RETURN(
	SELECT CONCAT(
	Tools.Number$Translate3DigitsToWords(SUBSTRING(@NumberToTranslateText,1,3)) + '-Trillion, '
	,
	 Tools.Number$Translate3DigitsToWords(SUBSTRING(@NumberToTranslateText,4,3)) + '-Billion, '
	,
	Tools.Number$Translate3DigitsToWords(SUBSTRING(@NumberToTranslateText,7,3)) + '-Million, '
	,
	 Tools.Number$Translate3DigitsToWords(SUBSTRING(@NumberToTranslateText,10,3)) + '-Thousand, '
	,	
	CASE WHEN @NumberToTranslate > 100 AND CAST(SUBSTRING(@NumberToTranslateText,13,3) AS int) < 100 THEN ' and ' END
	,
	Tools.Number$Translate3DigitsToWords(SUBSTRING(@NumberToTranslateText,13,3)))
	)
END
GO
CREATE OR ALTER FUNCTION Tools.Date$GetNthDay
(
	@DayName varchar(20), --Spelled out the name of the day of the week
	@NumberOfWeeks int = 0, --positive or negative offset from the current week
	@DateValue date = NULL --the day to start the calculation
)
---------------------------------------------
-- Example: Tuesday, 0, 2/17/2021 (Wednesday) Return: 2/23 
-- Example: Tuesday, 0, 2/17/2021 (Wednesday) Return: 03/02
-- Example: Wednesday, 0, 2/17/2021 (Wednesday) Return: 02/17
-- Example: Tuesday, -3, 2/17/2021 (Wednesday) Return: 1/27
/*
SELECT [Tools].[Date$GetNthDay] ('Tuesday', 0, '2021-02-17');
SELECT [Tools].[Date$GetNthDay] ('Tuesday', 1, '2021-02-17');
SELECT [Tools].[Date$GetNthDay] ('Wednesday', 0, '2021-02-17');
SELECT [Tools].[Date$GetNthDay] ('Wednesday', -3, '2021-02-17');
--a couple of fun tests
SELECT [Tools].[Date$GetNthDay] ('Wednesday', -1000, '2021-02-17');
SELECT [Tools].[Date$GetNthDay] ('Wednesday', 100000, '2021-02-17');
*/
---------------------------------------------

RETURNS date
AS
 BEGIN 
	--if the date parameter is NULL, use current date
	SET @dateValue = COALESCE(@DateValue,SYSDATETIME());

	--this is a stand in for a calendar table to make it portable and not subject
	--to any date settings
	DECLARE @DaysOfWeek table (DayNumber int NOT NULL, DayName varchar(20) NOT NULL);

	--load 14 days to make the math of days between days easy
	INSERT INTO @DaysOfWeek(DayNumber, DayName)
	VALUES(1,'Sunday'),(2,'Monday'),(3,'Tuesday'),(4,'Wednesday'),
		  (5,'Thursday'),(6,'Friday'),(7,'Saturday'),
		  (8,'Sunday'),(9,'Monday'),(10,'Tuesday'),(11,'Wednesday'),
		  (12,'Thursday'),(13,'Friday'),(14,'Saturday');


	--get the day number of the date that was passed in on the DateValue parameter
	DECLARE @CurrentDayNumber int = (SELECT MIN(DayNumber) 
									 FROM @DaysOfWeek 
									 WHERE DayName = DATENAME(weekday, @DateValue));  

	--get the next day number in the table to get the number of days to add
	DECLARE @NextDayNumber int = (SELECT MIN(DayNumber) 
								  FROM @DaysOfWeek 
								  WHERE DayName = @DayName 
								    AND DayNumber >= @CurrentDayNumber); 

	--add the number of weeks to the date you calculate to be the upcoming day that matched your parameters
	RETURN (DATEADD(WEEK,@NumberOfWeeks,DATEADD(DAY, @NextDayNumber - @CurrentDayNumber, @DateValue)));
 END;
GO

