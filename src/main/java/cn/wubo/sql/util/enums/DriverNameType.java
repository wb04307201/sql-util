package cn.wubo.sql.util.enums;

import com.alibaba.druid.DbType;

public enum DriverNameType {

    MYSQL("com.mysql.jdbc.Driver", DbType.mysql), ORACLE("oracle.jdbc.driver.OracleDriver", DbType.oracle), SQL_SERVER("com.microsoft.sqlserver.jdbc.SQLServerDriver", DbType.sqlserver), POSTGRESQL("org.postgresql.Driver", DbType.postgresql), H2("org.h2.Driver", DbType.h2), HSQL("org.hsqldb.jdbcDriver", DbType.hsql), DB2("com.ibm.db2.jcc.DB2Driver", DbType.db2), SYBASE("com.sybase.jdbc3.jdbc.SybDriver", DbType.sybase), DM("dm.jdbc.driver.DmDriver", DbType.dm);

    public String value;

    public DbType dbType;

    DriverNameType(String value, DbType dbType) {
        this.value = value;
        this.dbType = dbType;
    }

    public static DriverNameType getDriverNameType(String value) {
        for (DriverNameType driverNameType : DriverNameType.values()) {
            if (driverNameType.value.equals(value)) {
                return driverNameType;
            }
        }
        return null;
    }
}
