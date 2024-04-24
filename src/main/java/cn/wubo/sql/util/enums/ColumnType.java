package cn.wubo.sql.util.enums;

import lombok.Getter;

@Getter
public enum ColumnType {
    VARCHAR("VARCHAR"), DATE("DATE"), NUMBER("NUMBER"), TEXT("TEXT"), BLOB("BLOB");

    String value;

    ColumnType(String value) {
        this.value = value;
    }
}
