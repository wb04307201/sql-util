package cn.wubo.sql.util;

import cn.wubo.sql.util.exception.SQLRuntimeException;
import com.alibaba.druid.DbType;
import com.alibaba.druid.sql.PagerUtils;
import com.alibaba.druid.sql.SQLUtils;
import lombok.AllArgsConstructor;
import lombok.Data;
import lombok.Getter;

import java.lang.reflect.ParameterizedType;
import java.util.*;
import java.util.concurrent.atomic.AtomicInteger;
import java.util.stream.Collectors;

public class SQL<T> {
    private StatementType statementType;
    @Getter
    private String table;
    private List<String> columns = new ArrayList<>();
    private List<Set> sets = new ArrayList<>();
    private List<Where> wheres = new ArrayList<>();
    private AtomicInteger atomicInteger;
    @Getter
    private DbType dbType;

    @Getter
    private String parse;
    @Getter
    private Map<Integer, Object> params = new HashMap<>();

    @Getter
    private Class<T> clazz;

    public SQL(StatementType statementType) {
        this.statementType = statementType;
    }

    public SQL(StatementType statementType, List<String> columns) {
        this.statementType = statementType;
        this.columns = columns;
    }

    public SQL(StatementType statementType, List<String> columns, Class<T> clazz) {
        this.statementType = statementType;
        this.columns = columns;
        this.clazz = clazz;
    }

    public SQL(String table, DbType dbType) {
        this.table = table;
        this.dbType = dbType;
    }

    public static <T> SQL<T> select(String... columns) {
        return new SQL<>(StatementType.SELECT, Arrays.asList(columns));
    }

    public static <T> SQL<T> select(Class<T> clazz, String... columns) {
        return new SQL<>(StatementType.SELECT, Arrays.asList(columns), clazz);
    }

    public static <T> SQL<T> select(TypeReference<T> typeReference, String... columns) {
        return new SQL<>(StatementType.SELECT, Arrays.asList(columns), (Class<T>) ((ParameterizedType) typeReference.type).getRawType());
    }

    public static <T> SQL<T> insert() {
        return new SQL<>(StatementType.INSERT);
    }

    public static <T> SQL<T> update() {
        return new SQL<>(StatementType.UPDATE);
    }

    public static <T> SQL<T> delete() {
        return new SQL<>(StatementType.DELETE);
    }

    public static <T> SQL<T> tableExists(String table, DbType dbType) {
        return new SQL<>(table, dbType);
    }

    public SQL<T> table(String table) {
        this.table = table;
        return this;
    }

    public SQL<T> addSet(String field, Object value) {
        sets.add(new Set(field, value));
        return this;
    }

    public SQL<T> addWhereEQ(String field, Object value) {
        wheres.add(new Where(field, StatementCondition.EQ, value));
        return this;
    }

    public SQL<T> addWhereUEQ(String field, Object value) {
        wheres.add(new Where(field, StatementCondition.UEQ, value));
        return this;
    }

    public SQL<T> addWhereLIKE(String field, Object value) {
        wheres.add(new Where(field, StatementCondition.LIKE, value));
        return this;
    }

    public SQL<T> addWhereULIKE(String field, Object value) {
        wheres.add(new Where(field, StatementCondition.ULIKE, value));
        return this;
    }

    public SQL<T> addWhereLLIKE(String field, Object value) {
        wheres.add(new Where(field, StatementCondition.LLIKE, value));
        return this;
    }

    public SQL<T> addWhereRLIKE(String field, Object value) {
        wheres.add(new Where(field, StatementCondition.RLIKE, value));
        return this;
    }

    public SQL<T> addWhereGT(String field, Object value) {
        wheres.add(new Where(field, StatementCondition.GT, value));
        return this;
    }

    public SQL<T> addWhereLT(String field, Object value) {
        wheres.add(new Where(field, StatementCondition.LT, value));
        return this;
    }

    public SQL<T> addWhereGTEQ(String field, Object value) {
        wheres.add(new Where(field, StatementCondition.GTEQ, value));
        return this;
    }

    public SQL<T> addWhereLTEQ(String field, Object value) {
        wheres.add(new Where(field, StatementCondition.LTEQ, value));
        return this;
    }

    public SQL<T> addWhereBETWEEN(String field, Object value) {
        wheres.add(new Where(field, StatementCondition.BETWEEN, value));
        return this;
    }

    public SQL<T> addWhereNOTBETWEEN(String field, Object value) {
        wheres.add(new Where(field, StatementCondition.NOTBETWEEN, value));
        return this;
    }

    public SQL<T> addWhereIN(String field, Object value) {
        wheres.add(new Where(field, StatementCondition.IN, value));
        return this;
    }

    public SQL<T> addWhereNOTIN(String field, Object value) {
        wheres.add(new Where(field, StatementCondition.NOTIN, value));
        return this;
    }

    public SQL<T> addWhereNULL(String field) {
        wheres.add(new Where(field, StatementCondition.NULL));
        return this;
    }

    public SQL<T> addWhereNOTNULL(String field) {
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
        DELETE("DELETE "), INSERT("INSERT INTO "), SELECT("SELECT "), UPDATE("UPDATE ");

        private String value;

        StatementType(String value) {
            this.value = value;
        }

    }

    public enum StatementCondition {
        EQ(" = "), UEQ(" <> "), LIKE(" LIKE "), ULIKE(" NOT LIKE "), LLIKE(" LIKE "), RLIKE(" LIKE "), GT(" > "), LT(" < "), GTEQ(" >= "), LTEQ(" <= "), BETWEEN(" BETWEEN "), NOTBETWEEN(" NOT BETWEEN "), IN(" IN "), NOTIN(" NOT IN "), NULL(" IS NULL "), NOTNULL(" IS NOT NULL ");

        private String value;

        StatementCondition(String value) {
            this.value = value;
        }
    }

    public SQL<T> parse() {
        StringBuilder sb = new StringBuilder();
        atomicInteger = new AtomicInteger(0);
        switch (statementType) {
            case SELECT -> selectSQL(sb);
            case INSERT -> insertSQL(sb);
            case UPDATE -> updateSQL(sb);
            case DELETE -> deleteSQL(sb);
            default -> throw new IllegalArgumentException("Invalid statementType: " + statementType);
        }
        if (dbType != null) parse = SQLUtils.toSQLString(SQLUtils.parseStatements(sb.toString(), dbType), dbType);
        else parse = sb.toString();
        return this;
    }

    private void selectSQL(StringBuilder sb) {
        sb.append(statementType.value);
        if (!columns.isEmpty()) columns.forEach(str -> sb.append(str).append(","));
        else sb.append("*,");
        sb.delete(sb.length() - 1, sb.length()).append(" FROM ").append(table);
        whereSQL(sb);
    }

    private void whereSQL(StringBuilder sb) {
        String whereSQL = wheres.stream().map(where -> switch (where.getStatementCondition()) {
            case EQ, UEQ, GT, LT, GTEQ, LTEQ -> {
                params.put(atomicInteger.incrementAndGet(), where.getValue());
                yield where.getField() + where.getStatementCondition().value + "?";
            }
            case LIKE, ULIKE -> {
                String valueStr = "%" + where.getValue() + "%";
                params.put(atomicInteger.incrementAndGet(), valueStr);
                yield where.getField() + where.getStatementCondition().value + "?";
            }
            case LLIKE -> {
                String valueStr = "%" + where.getValue();
                params.put(atomicInteger.incrementAndGet(), valueStr);
                yield where.getField() + where.getStatementCondition().value + "?";
            }
            case RLIKE -> {
                String valueStr = where.getValue() + "%";
                params.put(atomicInteger.incrementAndGet(), valueStr);
                yield where.getField() + where.getStatementCondition().value + "?";
            }
            case BETWEEN, NOTBETWEEN -> {
                if (where.getValue() instanceof List valueObjs && valueObjs.size() == 2) {
                    params.put(atomicInteger.incrementAndGet(), valueObjs.get(0));
                    params.put(atomicInteger.incrementAndGet(), valueObjs.get(1));
                    yield where.getField() + where.getStatementCondition().value + "? AND ?";
                } else {
                    throw new SQLRuntimeException("recieving incomplete where condition between values invalid");
                }
            }
            case IN, NOTIN -> {
                if (where.getValue() instanceof List valueObjs) {
                    if (valueObjs.isEmpty()) {
                        yield "1 = 2";
                    } else {
                        valueObjs.forEach(valueObj -> params.put(atomicInteger.incrementAndGet(), valueObj));
                        yield where.getField() + where.getStatementCondition().value + "(" + valueObjs.stream().map(obj -> "?").collect(Collectors.joining(",")) + ")";
                    }
                } else {
                    throw new SQLRuntimeException("recieving incomplete where condition in values invalid");
                }
            }
            case NULL, NOTNULL -> where.getField() + where.getStatementCondition().value;
        }).collect(Collectors.joining(" AND "));
        if (!whereSQL.isEmpty()) sb.append(" WHERE ").append(whereSQL);
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

    public SQL<T> page(int offset, int count) {
        if (dbType == null)
            throw new SQLRuntimeException("before invoke page method,should invoke setDbtype to set dbtype!");
        this.parse = PagerUtils.limit(parse, dbType, offset, count);
        return this;
    }

    public SQL<T> setDbtype(DbType dbType) {
        this.dbType = dbType;
        if (parse != null && !parse.isEmpty())
            parse = SQLUtils.toSQLString(SQLUtils.parseStatements(parse, dbType), dbType);
        return this;
    }
}
