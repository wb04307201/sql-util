package cn.wubo.sql.util.enums;

import lombok.Getter;

@Getter
public enum EditType {

    TEXT("text"), NUMBER("number"), CHECKBOX("checkbox"), SELECT("select"), DATE("date");

    String value;

    EditType(String value) {
        this.value = value;
    }
}
