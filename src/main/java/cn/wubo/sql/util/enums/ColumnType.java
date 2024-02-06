package cn.wubo.sql.util.enums;

import lombok.Getter;

public enum ColumnType {
    VARCHAR("VARCHAR"), DATE("DATE"), NUMBER("number"), TEXT("TEXT"), BLOB("BLOB");

    @Getter
    String value;

    ColumnType(String value) {
        this.value = value;
    }


}
