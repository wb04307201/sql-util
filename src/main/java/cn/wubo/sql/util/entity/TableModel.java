package cn.wubo.sql.util.entity;

import cn.wubo.sql.util.annotations.Column;
import cn.wubo.sql.util.annotations.Condition;
import cn.wubo.sql.util.annotations.Ds;
import cn.wubo.sql.util.annotations.Key;
import cn.wubo.sql.util.enums.ColumnType;
import cn.wubo.sql.util.enums.EditType;
import cn.wubo.sql.util.enums.GenerationType;
import cn.wubo.sql.util.enums.StatementCondition;
import cn.wubo.sql.util.exception.EntityWebException;
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

@Getter
@ToString
public class TableModel {
    private String name;
    private String desc;
    private DsModel ds;
    private Boolean init;
    private List<ColumnModel> cols = new ArrayList<>();

    public TableModel(String name, String desc, Boolean init) {
        this.name = name;
        this.desc = desc;
        this.init = init;
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
                this.view = new ViewModel(column.view().show(), column.view().sortable(), column.view().exportable(), column.view().width(), column.view().translatable(), Arrays.stream(column.view().items()).map(item -> new ItemModel(item.value(), item.label())).toList());
                this.edit = new EditModel(column.edit().show(), column.edit().type(), column.edit().notNull(), column.edit().placeholder(), Arrays.stream(column.edit().items()).map(item -> new ItemModel(item.value(), item.label())).toList(), column.edit().search());
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
        public static class EditModel {
            private Boolean show;
            private EditType type;
            private Boolean notNull;
            private String placeholder;
            private List<ItemModel> items;
            private Boolean search;

            public EditModel(Boolean show, EditType type, Boolean notNull, String placeholder, List<ItemModel> items, Boolean search) {
                if (type == EditType.SELECT && (items == null || items.isEmpty()))
                    throw new EntityWebException("type为SELECT时，items不应为空！");
                if (type == EditType.CHECKBOX && (items == null || items.size() != 2))
                    throw new EntityWebException("type为CHECKBOX时，items应包含2个元素！");
                this.show = show;
                this.type = type;
                this.notNull = notNull;
                this.placeholder = placeholder;
                this.items = items;
                this.search = search;
            }
        }

        @Data
        @AllArgsConstructor
        public static class ItemModel {
            private String value;
            private String label;
        }

        /**
         * 根据字段类型转换为对应的数据库类型。
         * 该方法通过分析字段的类型，来确定在数据库中应该使用的对应数据类型。
         *
         * @param field 需要转换的字段对象。
         * @return 对应的数据库数据类型，如INTEGER、VARCHAR等。
         */
        private String fieldTypeToDbType(Field field) {
            // 根据字段类型，返回对应的数据库数据类型
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
                // 如果不是上述任何一种类型，则默认返回VARCHAR
                return "VARCHAR";
            }
        }

    }
}
