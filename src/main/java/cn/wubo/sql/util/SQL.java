package cn.wubo.sql.util;

import cn.wubo.sql.util.exception.ModelSqlUtilsException;
import lombok.AllArgsConstructor;
import lombok.Data;
import lombok.Getter;

import java.util.*;
import java.util.concurrent.atomic.AtomicInteger;
import java.util.stream.Collectors;

public class SQL {
    private StatementType statementType;
    private String table;
    private List<String> columns = new ArrayList<>();
    private List<Set> sets = new ArrayList<>();
    private List<Where> wheres = new ArrayList<>();
    AtomicInteger atomicInteger;

    @Getter
    private String sql;
    @Getter
    private Map<Integer, Object> params = new HashMap<>();

    public SQL(StatementType statementType) {
        this.statementType = statementType;
    }

    public SQL(StatementType statementType, List<String> columns) {
        this.statementType = statementType;
        this.columns = columns;
    }

    public static SQL select(String... columns) {
        return new SQL(StatementType.SELECT, Arrays.asList(columns));
    }

    public static SQL insert() {
        return new SQL(StatementType.INSERT);
    }

    public static SQL update() {
        return new SQL(StatementType.UPDATE);
    }

    public static SQL delete() {
        return new SQL(StatementType.DELETE);
    }

    public SQL table(String table) {
        this.table = table;
        return this;
    }

    public SQL addSet(String field, Object value) {
        sets.add(new Set(field, value));
        return this;
    }

    public SQL addWhereEQ(String field, Object value) {
        wheres.add(new Where(field, StatementCondition.EQ, value));
        return this;
    }

    public SQL addWhereUEQ(String field, Object value) {
        wheres.add(new Where(field, StatementCondition.UEQ, value));
        return this;
    }

    public SQL addWhereLIKE(String field, Object value) {
        wheres.add(new Where(field, StatementCondition.LIKE, value));
        return this;
    }

    public SQL addWhereULIKE(String field, Object value) {
        wheres.add(new Where(field, StatementCondition.ULIKE, value));
        return this;
    }

    public SQL addWhereLLIKE(String field, Object value) {
        wheres.add(new Where(field, StatementCondition.LLIKE, value));
        return this;
    }

    public SQL addWhereRLIKE(String field, Object value) {
        wheres.add(new Where(field, StatementCondition.RLIKE, value));
        return this;
    }

    public SQL addWhereGT(String field, Object value) {
        wheres.add(new Where(field, StatementCondition.GT, value));
        return this;
    }

    public SQL addWhereLT(String field, Object value) {
        wheres.add(new Where(field, StatementCondition.LT, value));
        return this;
    }

    public SQL addWhereGTEQ(String field, Object value) {
        wheres.add(new Where(field, StatementCondition.GTEQ, value));
        return this;
    }

    public SQL addWhereLTEQ(String field, Object value) {
        wheres.add(new Where(field, StatementCondition.LTEQ, value));
        return this;
    }

    public SQL addWhereBETWEEN(String field, Object value) {
        wheres.add(new Where(field, StatementCondition.BETWEEN, value));
        return this;
    }

    public SQL addWhereNOTBETWEEN(String field, Object value) {
        wheres.add(new Where(field, StatementCondition.NOTBETWEEN, value));
        return this;
    }

    public SQL addWhereIN(String field, Object value) {
        wheres.add(new Where(field, StatementCondition.IN, value));
        return this;
    }

    public SQL addWhereNOTIN(String field, Object value) {
        wheres.add(new Where(field, StatementCondition.NOTIN, value));
        return this;
    }

    public SQL addWhereNULL(String field) {
        wheres.add(new Where(field, StatementCondition.NULL));
        return this;
    }

    public SQL addWhereNOTNULL(String field) {
        wheres.add(new Where(field, StatementCondition.NOTNULL));
        return this;
    }

    @Data
    @AllArgsConstructor
    public static class Set {
        private String field;
        private Object value;
    }

    @Data
    @AllArgsConstructor
    public static class Where {
        private String field;
        private StatementCondition statementCondition;
        private Object value;

        public Where(String field, StatementCondition statementCondition) {
            this.field = field;
            this.statementCondition = statementCondition;
        }
    }

    public enum StatementType {
        DELETE("delete "), INSERT("inser into "), SELECT("select "), UPDATE("update ");

        private String value;

        StatementType(String value) {
            this.value = value;
        }

    }

    public enum StatementCondition {
        EQ(" = "), UEQ(" <> "), LIKE(" like "), ULIKE(" not like "), LLIKE(" like "), RLIKE(" like "), GT(" > "), LT(" < "), GTEQ(" >= "), LTEQ(" <= "), BETWEEN(" between "), NOTBETWEEN(" not between "), IN(" in "), NOTIN(" not in "), NULL(" is null "), NOTNULL(" is not null ");

        private String value;

        StatementCondition(String value) {
            this.value = value;
        }
    }

    public SQL parse() {
        StringBuilder sb = new StringBuilder();
        atomicInteger = new AtomicInteger(0);
        switch (statementType) {
            case SELECT:
                selectSQL(sb);
                break;
            case INSERT:
                insertSQL(sb);
                break;
            case UPDATE:
                updateSQL(sb);
                break;
            case DELETE:
                deleteSQL(sb);
                break;
            default:
        }
        sql = sb.toString();
        return this;
    }

    private void selectSQL(StringBuilder sb) {
        sb.append(statementType.value);
        if (!columns.isEmpty()) columns.forEach(str -> sb.append(str).append(","));
        else sb.append("*,");
        sb.delete(sb.length() - 1, sb.length()).append(" from ").append(table);
        whereSQL(sb);
    }

    private void whereSQL(StringBuilder sb) {
        sb.append(" WHERE ").append(wheres.stream().map(where -> {
            if (where.getStatementCondition() == StatementCondition.EQ || where.getStatementCondition() == StatementCondition.UEQ || where.getStatementCondition() == StatementCondition.GT || where.getStatementCondition() == StatementCondition.LT || where.getStatementCondition() == StatementCondition.GTEQ || where.getStatementCondition() == StatementCondition.LTEQ) {
                params.put(atomicInteger.incrementAndGet(), where.getValue());
                return where.getField() + where.getStatementCondition().value + "?";
            } else if (where.getStatementCondition() == StatementCondition.LIKE || where.getStatementCondition() == StatementCondition.ULIKE) {
                String valueStr = "%" + where.getValue() + "%";
                params.put(atomicInteger.incrementAndGet(), valueStr);
                return where.getField() + where.getStatementCondition().value + "?";
            } else if (where.getStatementCondition() == StatementCondition.LLIKE) {
                String valueStr = "%" + where.getValue();
                params.put(atomicInteger.incrementAndGet(), valueStr);
                return where.getField() + where.getStatementCondition().value + "?";
            } else if (where.getStatementCondition() == StatementCondition.RLIKE) {
                String valueStr = where.getValue() + "%";
                params.put(atomicInteger.incrementAndGet(), valueStr);
                return where.getField() + where.getStatementCondition().value + "?";
            } else if (where.getStatementCondition() == StatementCondition.BETWEEN || where.getStatementCondition() == StatementCondition.NOTBETWEEN) {
                if (where.getValue() instanceof List && ((List<Object>) where.getValue()).size() == 2) {
                    List<Object> valueObjs = (List<Object>) where.getValue();
                    params.put(atomicInteger.incrementAndGet(), valueObjs.get(0));
                    params.put(atomicInteger.incrementAndGet(), valueObjs.get(1));
                    return where.getField() + where.getStatementCondition().value + "? AND ?";
                } else {
                    throw new ModelSqlUtilsException("recieving incomplete @where condition between values invalid");
                }
            } else if (where.getStatementCondition() == StatementCondition.IN || where.getStatementCondition() == StatementCondition.NOTIN) {
                if (where.getValue() instanceof List) {
                    List<Object> valueObjs = (List<Object>) where.getValue();
                    if (valueObjs.isEmpty()) {
                        return "1 = 2";
                    } else {
                        valueObjs.forEach(valueObj -> params.put(atomicInteger.incrementAndGet(), valueObj));
                        return where.getField() + where.getStatementCondition().value + "(" + valueObjs.stream().map(obj -> "?").collect(Collectors.joining(",")) + ")";
                    }
                } else {
                    throw new ModelSqlUtilsException("recieving incomplete @where condition in values invalid");
                }
            } else if (where.getStatementCondition() == StatementCondition.NULL || where.getStatementCondition() == StatementCondition.NOTNULL) {
                return where.getField() + where.getStatementCondition().value;
            } else {
                return null;
            }
        }).filter(Objects::nonNull).collect(Collectors.joining(" AND ")));
    }

    private void insertSQL(StringBuilder sb) {
        sb.append(statementType.value).append(table).append(" (").append(sets.stream().map(set -> {
            params.put(atomicInteger.incrementAndGet(), set.getValue());
            return set.getField();
        }).collect(Collectors.joining(","))).append(") VALUES (").append(sets.stream().map(set -> "?").collect(Collectors.joining(","))).append(")");
    }

    private void updateSQL(StringBuilder sb) {
        sb.append(statementType.value).append(table).append(" SET ").append(sets.stream().map(set -> {
            params.put(atomicInteger.incrementAndGet(), set.getValue());
            return set.getField() + " = ?";
        }).collect(Collectors.joining(",")));
        whereSQL(sb);
    }

    private void deleteSQL(StringBuilder sb) {
        sb.append(statementType.value).append(table);
        whereSQL(sb);
    }
}
