package cn.wubo.sql.util.enums;

import lombok.Getter;

public enum EditType {

    TEXT("text"), NUMBER("number"), CHECKBOX("checkbox"), SELECT("select"), DATE("date");

    @Getter
    String value;

    EditType(String value) {
        this.value = value;
    }
}
