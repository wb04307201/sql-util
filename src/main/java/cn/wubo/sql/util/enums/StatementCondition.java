package cn.wubo.sql.util.enums;

import lombok.Getter;

public enum StatementCondition {

    EQ(" = "), UEQ(" <> "), LIKE(" LIKE "), ULIKE(" NOT LIKE "), LLIKE(" LIKE "), RLIKE(" LIKE "), GT(" > "), LT(" < "), GTEQ(" >= "), LTEQ(" <= "), BETWEEN(" BETWEEN "), NOTBETWEEN(" NOT BETWEEN "), IN(" IN "), NOTIN(" NOT IN "), NULL(" IS NULL "), NOTNULL(" IS NOT NULL ");

    @Getter
    String value;

    StatementCondition(String value) {
        this.value = value;
    }
}
