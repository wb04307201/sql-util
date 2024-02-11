package cn.wubo.sql.util.entity;

import cn.wubo.sql.util.annotations.Column;
import cn.wubo.sql.util.annotations.Condition;
import cn.wubo.sql.util.annotations.Ds;
import cn.wubo.sql.util.annotations.Key;
import cn.wubo.sql.util.enums.ColumnType;
import cn.wubo.sql.util.enums.EditType;
import cn.wubo.sql.util.enums.GenerationType;
import cn.wubo.sql.util.enums.StatementCondition;
import cn.wubo.sql.util.utils.StringUtils;
import lombok.AllArgsConstructor;
import lombok.Data;
import lombok.Getter;
import lombok.ToString;

import java.lang.annotation.Annotation;
import java.lang.reflect.Field;
import java.math.BigDecimal;
import java.sql.Blob;
import java.sql.Clob;
import java.sql.Time;
import java.sql.Timestamp;
import java.util.*;
import java.util.stream.Collectors;

@Getter
@ToString
public class TableModel {
    private String name;
    private String desc;
    private DsModel ds;
    private List<ColumnModel> cols = new ArrayList<>();

    public TableModel(String name, String desc) {
        this.name = name;
        this.desc = desc;
    }

    public TableModel setDs(Ds ds) {
        this.ds = new DsModel(ds.url(), ds.username(), ds.passowrd());
        return this;
    }

    public TableModel addColumns(List<ColumnModel> cols) {
        this.cols.addAll(cols);
        return this;
    }

    @Data
    @ToString
    @AllArgsConstructor
    public static class DsModel {
        private String url;
        private String username;
        private String password;
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
        private Integer sort;

        private String definition;

        private Boolean key = false;
        private GenerationType generationType;

        private StatementCondition statementCondition;

        private Field field;

        private ViewModel view;

        private EditModel edit;

        public ColumnModel(Field field) {
            this.fieldName = field.getName();
            this.field = field;

            Annotation[] fieldAnns = field.getAnnotations();
            Optional<Annotation> columnAnnOpt = Arrays.stream(fieldAnns).filter(Column.class::isInstance).findAny();
            if (columnAnnOpt.isPresent()) {
                Column column = (Column) columnAnnOpt.get();
                this.columnName = column.value();
                this.desc = StringUtils.defaultValue(column.desc(), field.getName());
                this.type = column.type().getValue();
                this.definition = this.type;
                this.sort = column.sort();
                if (column.type() == ColumnType.VARCHAR) {
                    this.length = column.length();
                    if (this.length != 0) this.definition = this.type + "(" + this.length + ")";
                } else if (column.type() == ColumnType.NUMBER) {
                    this.precision = column.precision();
                    this.scale = column.scale();
                    if (this.precision != 0 && this.scale != 0)
                        this.definition = this.type + "(" + this.precision + "," + this.scale + ")";
                    else if (this.precision != 0) this.definition = this.type + "(" + this.precision + ")";
                }
                this.view = new ViewModel(column.view().show(), column.view().sortable(), column.view().exportable(), column.view().width(), column.view().translatable(), Arrays.stream(column.view().items()).map(item -> new ItemModel(item.value(), item.label())).collect(Collectors.toList()));
                this.edit = new EditModel(column.edit().show(), column.edit().type(), column.edit().notNull(), column.edit().placeholder(), Arrays.stream(column.edit().items()).map(item -> new ItemModel(item.value(), item.label())).collect(Collectors.toList()), column.edit().search());
            } else {
                this.columnName = field.getName();
                this.desc = field.getName();
                this.type = fieldTypeToDbType(field);
                this.definition = this.type;
                this.sort = 100;
            }

            Arrays.stream(fieldAnns).filter(Key.class::isInstance).findAny().ifPresent(ann -> {
                Key tempKey = (Key) ann;
                this.key = true;
                this.generationType = tempKey.value();
            });

            Arrays.stream(fieldAnns).filter(Condition.class::isInstance).findAny().ifPresent(ann -> {
                Condition condition = (Condition) ann;
                this.statementCondition = condition.value();
            });
        }

        @Data
        @AllArgsConstructor
        public static class ViewModel {
            private Boolean show;
            private Boolean sortable;
            private Boolean exportable;
            private Integer width;
            private Boolean translatable;
            private List<ItemModel> items;
        }

        @Data
        @AllArgsConstructor
        public static class EditModel {
            private Boolean show;
            private EditType type;
            private Boolean notNull;
            private String placeholder;
            private List<ItemModel> items;
            private Boolean search;
        }

        @Data
        @AllArgsConstructor
        public static class ItemModel {
            private String value;
            private String label;
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
