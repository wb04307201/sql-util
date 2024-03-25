package cn.wubo.sql.util.enums;

import lombok.Getter;

@Getter
public enum StatementType {
    DELETE("DELETE "), INSERT("INSERT INTO "), SELECT("SELECT "), UPDATE("UPDATE "),CREATE("CREATE TABLE"),DROP("DROP TABLE");

    String value;

    StatementType(String value) {
        this.value = value;
    }
}
