# lazarus-batis-utility
Database Entity to Objects <-> Object to Database Entity 
# IT-it
Partendo da un file di configurazione in formato xml si connette, il programma si connette al database designato e genera le classi entity per mappare le entità di relazionali e i relativi mapper per gestire la persistenza di queste entità.
# EN-us
Starting from a configuration file in xml format, the program connects to the designated database and generates the entity classes to map the relational entities and their mappers to manage the persistence of these entities.
#### XML File
Example 1: Simple configuration
```xml
<?xml version="1.0" encoding="UTF-8"?>
<generator-configuration>
	<context id="context1 id" target-location="common position">
		<copyright>
			<![CDATA[ Free Text ]]>
		</copyright>
		<zdbc-connection connection-url="zeos lib connection string" username="username" password="password" [skip="regular expression"]></zdbc-connection>
	</context>
<generator-configuration>
```

The program attach database connection and load all relation entity from default schema. For each entity generate an interface, a concrete class (private for compilation unit) and a mapper.
Default name assigned to every entity follow this schema:
- I<EntityName>
- T<EntityName>
- T<EntityName>Mapper

Example 2: More configuration
```xml
<?xml version="1.0" encoding="UTF-8"?>
<generator-configuration>
	<context id="context id 1" target-location="common location">
		<copyright>
			<![CDATA[ (C) 2018 ACME Corporation ]]>
		</copyright>
		<zdbc-connection connection-url="connection utr" username="username" password="password"></zdbc-connection>
		<!-- module 1  -->
		<table table-name="Users" mapper-name="TUsersMapper" entity-name="IUser" implementation-name="TUser" target-module="module.user">
			<mapper>
				<select name="selectUserGrants">
					<parameter name="user" type="IUser"/>
					<return type='IUser'/>
					<![CDATA[ SELECT ID, DELETED, CREATION_DATE, UPDATE_DATE, NAME FROM USER_PERMISSION WHERE UID=${user.id} ]]>
				</select>
				<select name="findLogonUser">
					<parameter name="username" type="String"/>
					<parameter name="password" type="String"/>
					<return type='IUser'/>
					<![CDATA[ SELECT ID, USERNAME, PASSWORD FROM USER_TABLE WHERE USERNAME=${username} AND PASSWORD=${password} ]]>
				</select>
			</mapper>
		</table>
		<table table-name="Permission" mapper-name="TUserMapper" entity-name="IPermission" implementation-name="TPermission" target-module="module.user"/>
		<table table-name="UserGroup" mapper-name="TUserMapper" entity-name="IGroup" implementation-name="TGroup" target-module="module.user"/>
		<table table-name="tblxyz" skip="true"/>
		<table table-name="ISO$3166" mapper-name="TIso3166Mapper" entity-name="IIso3166" implementation-name="TIso3166" target-module="module">
			<column column-name="CODE2" model-name="Code2"/>
			<column column-name="CODE3" model-name="Code3" skip="true"/>
			<column column-name="NAME" model-name="Name"/>
			<column column-name="numeric" model-name="Numeric"/>
		</table>
		<table table-name="Table1" mapper-name="TTable1Mapper" entity-name="ITable1" implementation-name="TTable1" target-module="module.other"></table>
	</context>
</generator-configuration>
```
In this example we define two custom methods for mapper called TUserMapper. We define input parameters and return type.
The sql statemente use ${parmetername} as macro to expand in sql generation phase.
No validity check was implemente on ${parmetername}, we trust in user. 
The "parametername" must be valid expression for freepascal compiler. Like example before.
username and password are valid statements for freepascl.
user.id is valid too.
For a mapper default method are:
- save<entityname>(value: I<entityname>)
- get<entityname>(primary key definition on database)
- delete<entityname>(value : I<entityname>)
- create<entityname>(value : I<entityname>)

