package cn.wubo.sql.util.entity;

import cn.wubo.sql.util.annotations.Column;
import cn.wubo.sql.util.enums.ColumnType;
import cn.wubo.sql.util.enums.GenerationType;
import cn.wubo.sql.util.enums.StatementCondition;
import lombok.Data;
import lombok.Getter;

import java.lang.annotation.Annotation;
import java.lang.reflect.Field;
import java.math.BigDecimal;
import java.sql.Blob;
import java.sql.Clob;
import java.sql.Time;
import java.sql.Timestamp;
import java.util.*;

@Getter
public class TableModel {
    private String name;
    private String desc;
    private List<ColumnModel> cols = new ArrayList<>();

    public TableModel(String name, String desc) {
    }

    public TableModel addColumns(List<ColumnModel> cols) {
        this.cols.addAll(cols);
        return this;
    }

    @Data
    public static class ColumnModel {
        private String fieldName;
        private String columnName;
        private String desc;
        private String type;
        private Integer length;
        private Integer precision;
        private Integer scale;

        private Boolean id;
        private GenerationType generationType;

        private StatementCondition statementCondition;

        public ColumnModel(Field field) {
            this.fieldName = field.getName();

            Annotation[] fieldAnns = field.getAnnotations();
            Optional<Annotation> fieldAnnOpt = Arrays.stream(fieldAnns).filter(cn.wubo.sql.util.annotations.Column.class::isInstance).findAny();
            if (fieldAnnOpt.isPresent()) {
                Column column = (Column) fieldAnnOpt.get();
                this.columnName = column.value();
                this.desc = column.desc();
                this.type = column.type().getValue();
                if (column.type() == ColumnType.VARCHAR) {
                    this.length = column.length();
                } else if (column.type() == ColumnType.NUMBER) {
                    this.precision = column.precision();
                    this.scale = column.scale();
                }
            } else {
                this.columnName = field.getName();
                this.desc = field.getName();
                this.type = fieldTypeToDbType(field);
            }
        }

        /**
         * 根据字段类型转换为数据库类型
         *
         * @param field 字段对象
         * @return 数据库类型
         */
        private String fieldTypeToDbType(Field field) {
            // 判断字段类型
            if (field.getType().equals(Integer.class)) {
                return "INTEGER";
            } else if (field.getType().equals(Long.class)) {
                return "BIGINT";
            } else if (field.getType().equals(Float.class)) {
                return "FLOAT";
            } else if (field.getType().equals(Double.class)) {
                return "DOUBLE";
            } else if (field.getType().equals(BigDecimal.class)) {
                return "NUMBER";
            } else if (field.getType().equals(java.util.Date.class) || field.getType().equals(java.sql.Date.class)) {
                return "DATE";
            } else if (field.getType().equals(Time.class)) {
                return "TIME";
            } else if (field.getType().equals(Timestamp.class) || field.getType().equals(Calendar.class)) {
                return "TIMESTAMP";
            } else if (field.getType().equals(Boolean.class)) {
                return "BIT";
            } else if (field.getType().equals(Blob.class)) {
                return "BLOB";
            } else if (field.getType().equals(Clob.class)) {
                return "CLOB";
            } else {
                return "VARCHAR";
            }
        }
    }
}
