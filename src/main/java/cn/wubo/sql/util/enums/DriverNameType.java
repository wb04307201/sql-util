package cn.wubo.sql.util.enums;

import com.alibaba.druid.DbType;
import lombok.Getter;

@Getter
public enum DriverNameType {

    MYSQL("com.mysql.jdbc.Driver", "jdbc:mysql:", "mysql", DbType.mysql), ORACLE("oracle.jdbc.driver.OracleDriver", "jdbc:oracle:", "oracle", DbType.oracle), SQL_SERVER("com.microsoft.sqlserver.jdbc.SQLServerDriver", "sqlserver", "jdbc:sqlserver:", DbType.sqlserver), POSTGRESQL("org.postgresql.Driver", "jdbc:postgresql:", "postgresql", DbType.postgresql), H2("org.h2.Driver", "jdbc:h2:", "h2", DbType.h2), DM("dm.jdbc.driver.DmDriver", "jdbc:dm:", "dm", DbType.dm);

    private String value;
    private String url;
    private String name;
    private DbType dbType;

    DriverNameType(String value, String url, String name, DbType dbType) {
        this.value = value;
        this.url = url;
        this.name = name;
        this.dbType = dbType;
    }

    public static DbType getDriverNameType(String className) {
        for (DriverNameType driverNameType : DriverNameType.values()) {
            if (driverNameType.value.equals(className)) {
                return driverNameType.dbType;
            }
        }
        return null;
    }

    public static DbType getDriverNameTypeFromMeta(String driverName) {
        driverName = driverName.toLowerCase();
        for (DriverNameType driverNameType : DriverNameType.values()) {
            if (driverName.contains(driverNameType.getName())) {
                return driverNameType.dbType;
            }
        }
        return null;
    }

    public static DbType getDriverNameTypeFromUrl(String url) {
        url = url.toLowerCase();
        for (DriverNameType driverNameType : DriverNameType.values()) {
            if (url.startsWith(driverNameType.getName())) {
                return driverNameType.dbType;
            }
        }
        return null;
    }
}
