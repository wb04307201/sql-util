package cn.wubo.sql.util.enums;

import lombok.Getter;

public enum StatementType {
    DELETE("DELETE "), INSERT("INSERT INTO "), SELECT("SELECT "), UPDATE("UPDATE ");

    @Getter
    String value;

    StatementType(String value) {
        this.value = value;
    }

}
