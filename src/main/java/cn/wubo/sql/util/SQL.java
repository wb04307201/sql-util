package cn.wubo.sql.util;

import cn.wubo.sql.util.enums.StatementCondition;
import cn.wubo.sql.util.enums.StatementType;
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

    /**
     * 构造一个SELECT语句
     *
     * @param columns 要查询的列
     * @return SQL对象
     */
    public static <T> SQL<T> select(String... columns) {
        return new SQL<>(StatementType.SELECT, Arrays.asList(columns));
    }

    /**
     * 构造一个SELECT语句
     *
     * @param clazz 要查询的实体类
     * @param columns 要查询的列
     * @return SQL对象
     */
    public static <T> SQL<T> select(Class<T> clazz, String... columns) {
        return new SQL<>(StatementType.SELECT, Arrays.asList(columns), clazz);
    }

    /**
     * 构造一个SELECT语句
     *
     * @param typeReference 要查询的实体类的类型引用
     * @param columns 要查询的列
     * @return SQL对象
     */
    public static <T> SQL<T> select(TypeReference<T> typeReference, String... columns) {
        return new SQL<>(StatementType.SELECT, Arrays.asList(columns), (Class<T>) ((ParameterizedType) typeReference.type).getRawType());
    }

    /**
     * 构造一个用于插入数据的SQL对象
     * @param <T> 数据类型
     * @return SQL对象
     */
    public static <T> SQL<T> insert() {
        return new SQL<>(StatementType.INSERT);
    }


    /**
     * 构造一个更新语句的SQL对象
     * @param <T> 泛型参数
     * @return SQL对象
     */
    public static <T> SQL<T> update() {
        return new SQL<>(StatementType.UPDATE);
    }


    /**
     * 构造一个删除语句的SQL对象
     * @param <T> 泛型参数
     * @return SQL对象
     */
    public static <T> SQL<T> delete() {
        return new SQL<>(StatementType.DELETE);
    }


    /**
     * 构造一个SQL对象，用于检查指定表是否存在
     *
     * @param table 表名
     * @param dbType 数据库类型
     * @return SQL对象
     */
    public static <T> SQL<T> tableExists(String table, DbType dbType) {
        return new SQL<>(table, dbType);
    }


    public SQL<T> table(String table) {
        this.table = table;
        return this;
    }

    /**
     * 添加一个设置到SQL对象中
     *
     * @param field 字段名
     * @param value 值
     * @return SQL对象
     */
    public SQL<T> addSet(String field, Object value) {
        sets.add(new Set(field, value));
        return this;
    }


    /**
     * 添加一个等值条件到SQL对象中
     *
     * @param field 字段名
     * @param value 值
     * @return SQL对象
     */
    public SQL<T> addWhereEQ(String field, Object value) {
        wheres.add(new Where(field, StatementCondition.EQ, value));
        return this;
    }


    /**
     * 添加一个等值不等于的条件到WHERE子句中。
     *
     * @param field 字段名
     * @param value 值
     * @return SQL对象
     */
    public SQL<T> addWhereUEQ(String field, Object value) {
        wheres.add(new Where(field, StatementCondition.UEQ, value));
        return this;
    }

    /**
     * 添加一个LIKE条件到WHERE子句中。
     *
     * @param field 字段名
     * @param value 值
     * @return SQL对象
     */
    public SQL<T> addWhereLIKE(String field, Object value) {
        wheres.add(new Where(field, StatementCondition.LIKE, value));
        return this;
    }

    /**
     * 添加一个以"LIKE"条件的WHERE子句
     * @param field 字段名
     * @param value 值
     * @return SQL对象
     */
    public SQL<T> addWhereULIKE(String field, Object value) {
        // 添加一个以"ULIKE"条件的WHERE子句
        wheres.add(new Where(field, StatementCondition.ULIKE, value));
        return this;
    }

    /**
     * 添加一个以"LIKE"条件的WHERE子句
     * @param field 字段名
     * @param value 值
     * @return SQL对象
     */
    public SQL<T> addWhereLLIKE(String field, Object value) {
        // 添加一个以"LLIKE"条件的WHERE子句
        wheres.add(new Where(field, StatementCondition.LLIKE, value));
        return this;
    }

    /**
     * 添加一个RLIKE条件到WHERE子句中
     * @param field 字段名
     * @param value 匹配的值
     * @return SQL对象
     */
    public SQL<T> addWhereRLIKE(String field, Object value) {
        // 添加一个Where对象到wheres列表中
        wheres.add(new Where(field, StatementCondition.RLIKE, value));
        return this;
    }

    /**
     * 添加一个GT条件到WHERE子句中
     * @param field 字段名
     * @param value 大于的值
     * @return SQL对象
     */
    public SQL<T> addWhereGT(String field, Object value) {
        // 添加一个Where对象到wheres列表中
        wheres.add(new Where(field, StatementCondition.GT, value));
        return this;
    }

    /**
     * 添加一个LT条件到WHERE子句中
     * @param field 字段名
     * @param value 小于的值
     * @return SQL对象
     */
    public SQL<T> addWhereLT(String field, Object value) {
        // 添加一个Where对象到wheres列表中
        wheres.add(new Where(field, StatementCondition.LT, value));
        return this;
    }

    /**
     * 添加一个GTEQ条件到WHERE子句中
     * @param field 字段名
     * @param value 大于等于的值
     * @return SQL对象
     */
    public SQL<T> addWhereGTEQ(String field, Object value) {
        // 添加一个Where对象到wheres列表中
        wheres.add(new Where(field, StatementCondition.GTEQ, value));
        return this;
    }

    /**
     * 添加一个LTEQ条件到WHERE子句中
     * @param field 字段名
     * @param value 小于等于的值
     * @return SQL对象
     */
    public SQL<T> addWhereLTEQ(String field, Object value) {
        // 添加一个Where对象到wheres列表中
        wheres.add(new Where(field, StatementCondition.LTEQ, value));
        return this;
    }

    /**
     * 添加一个BETWEEN条件到WHERE子句中
     * @param field 字段名
     * @param value 值
     * @return SQL对象
     */
    public SQL<T> addWhereBETWEEN(String field, Object value) {
        // 添加一个BETWEEN条件到WHERE子句中
        wheres.add(new Where(field, StatementCondition.BETWEEN, value));
        return this;
    }

    /**
     * 添加一个NOT BETWEEN条件到WHERE子句中
     * @param field 字段名
     * @param value 值
     * @return SQL对象
     */
    public SQL<T> addWhereNOTBETWEEN(String field, Object value) {
        // 添加一个NOT BETWEEN条件到WHERE子句中
        wheres.add(new Where(field, StatementCondition.NOTBETWEEN, value));
        return this;
    }

    /**
     * 添加一个IN条件到WHERE子句中
     * @param field 字段名
     * @param value 值
     * @return SQL对象
     */
    public SQL<T> addWhereIN(String field, Object value) {
        // 添加一个IN条件到WHERE子句中
        wheres.add(new Where(field, StatementCondition.IN, value));
        return this;
    }

    /**
     * 添加一个NOT IN条件到WHERE子句中
     * @param field 字段名
     * @param value 值
     * @return SQL对象
     */
    public SQL<T> addWhereNOTIN(String field, Object value) {
        // 添加一个NOT IN条件到WHERE子句中
        wheres.add(new Where(field, StatementCondition.NOTIN, value));
        return this;
    }

    /**
     * 添加一个字段为NULL的WHERE条件
     * @param field 字段名
     * @return SQL对象
     */
    public SQL<T> addWhereNULL(String field) {
        wheres.add(new Where(field, StatementCondition.NULL));
        return this;
    }

    /**
     * 添加一个WHERE NOT NULL条件
     * @param field 字段名
     * @return SQL对象
     */
    public SQL<T> addWhereNOTNULL(String field) {
        // 添加一个WHERE NOT NULL条件
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

    public SQL<T> parse() {
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
        if (dbType != null) parse = SQLUtils.toSQLString(SQLUtils.parseStatements(sb.toString(), dbType), dbType);
        else parse = sb.toString();
        return this;
    }

    /**
     * 构建SELECT语句
     * @param sb SQL语句的构建器
     */
    private void selectSQL(StringBuilder sb) {
        // 添加SELECT语句的类型
        sb.append(statementType.getValue());
        // 如果columns不为空，则遍历columns列表，将每个字段名添加到sb中，并在每个字段名后面添加逗号
        if (!columns.isEmpty()) columns.forEach(str -> sb.append(str).append(","));
        // 如果columns为空，则添加"*"和逗号
        else sb.append("*,");
        // 删除最后一个逗号，并添加FROM和table
        sb.delete(sb.length() - 1, sb.length()).append(" FROM ").append(table);
        // 添加WHERE语句
        whereSQL(sb);
    }

    /**
     * Generates the SQL WHERE clause based on the given list of wheres.
     *
     * @param sb the StringBuilder to append the generated SQL WHERE clause to
     */
    private void whereSQL(StringBuilder sb) {
        String whereSQL = wheres.stream().map(where -> {
            // Check if the condition is an OR condition
            if (Boolean.TRUE.equals(FunctionUtils.compileConditionOr(where, t -> where.getStatementCondition() == StatementCondition.EQ, t -> where.getStatementCondition() == StatementCondition.UEQ, t -> where.getStatementCondition() == StatementCondition.GT, t -> where.getStatementCondition() == StatementCondition.GTEQ, t -> where.getStatementCondition() == StatementCondition.LT, t -> where.getStatementCondition() == StatementCondition.LTEQ))) {
                params.put(atomicInteger.incrementAndGet(), where.getValue());
                return where.getField() + where.getStatementCondition().getValue() + "?";
            } else if (Boolean.TRUE.equals(FunctionUtils.compileConditionOr(where, t -> where.getStatementCondition() == StatementCondition.LIKE, t -> where.getStatementCondition() == StatementCondition.ULIKE))) {
                String valueStr = "%" + where.getValue() + "%";
                params.put(atomicInteger.incrementAndGet(), valueStr);
                return where.getField() + where.getStatementCondition().getValue() + "?";
            } else if (where.getStatementCondition() == StatementCondition.LLIKE) {
                String valueStr = "%" + where.getValue();
                params.put(atomicInteger.incrementAndGet(), valueStr);
                return where.getField() + where.getStatementCondition().getValue() + "?";
            } else if (where.getStatementCondition() == StatementCondition.RLIKE) {
                String valueStr = where.getValue() + "%";
                params.put(atomicInteger.incrementAndGet(), valueStr);
                return where.getField() + where.getStatementCondition().getValue() + "?";
            } else if (Boolean.TRUE.equals(FunctionUtils.compileConditionOr(where, t -> where.getStatementCondition() == StatementCondition.BETWEEN, t -> where.getStatementCondition() == StatementCondition.NOTBETWEEN))) {
                return FunctionUtils.buildCondition(where, t -> FunctionUtils.compileConditionAnd(t, tt -> tt.getValue() instanceof List, tt -> ((List<?>) tt.getValue()).size() == 2), t -> {
                    List<?> valueObjs = (List<?>) where.getValue();
                    params.put(atomicInteger.incrementAndGet(), valueObjs.get(0));
                    params.put(atomicInteger.incrementAndGet(), valueObjs.get(1));
                    return where.getField() + where.getStatementCondition().getValue() + "? AND ?";
                });
            } else if (Boolean.TRUE.equals(FunctionUtils.compileConditionOr(where, t -> where.getStatementCondition() == StatementCondition.IN, t -> where.getStatementCondition() == StatementCondition.NOTIN))) {
                return FunctionUtils.buildCondition(where, t -> where.getValue() instanceof List, t -> {
                    List<?> valueObjs = (List<?>) where.getValue();
                    if (valueObjs.isEmpty()) {
                        return "1 = 2";
                    } else {
                        valueObjs.forEach(valueObj -> params.put(atomicInteger.incrementAndGet(), valueObj));
                        return where.getField() + where.getStatementCondition().getValue() + "(" + valueObjs.stream().map(obj -> "?").collect(Collectors.joining(",")) + ")";
                    }
                });
            } else if (Boolean.TRUE.equals(FunctionUtils.compileConditionOr(where, t -> where.getStatementCondition() == StatementCondition.NULL, t -> where.getStatementCondition() == StatementCondition.NOTNULL))) {
                return where.getField() + where.getStatementCondition().getValue();
            } else {
                return null;
            }
        }).filter(Objects::nonNull).collect(Collectors.joining(" AND "));
        if (!whereSQL.isEmpty()) sb.append(" WHERE ").append(whereSQL);
    }

    /**
     * Generates the SQL INSERT statement based on the given sets.
     *
     * @param sb the StringBuilder to append the generated SQL INSERT statement to
     */
    private void insertSQL(StringBuilder sb) {
        sb.append(statementType.getValue()).append(table).append(" (").append(sets.stream().map(set -> {
            params.put(atomicInteger.incrementAndGet(), set.getValue());
            return set.getField();
        }).collect(Collectors.joining(","))).append(") VALUES (").append(sets.stream().map(set -> "?").collect(Collectors.joining(","))).append(")");
    }

    /**
     * Generates the SQL UPDATE statement based on the given sets.
     *
     * @param sb the StringBuilder to append the generated SQL UPDATE statement to
     */
    private void updateSQL(StringBuilder sb) {
        sb.append(statementType.getValue()).append(table).append(" SET ").append(sets.stream().map(set -> {
            params.put(atomicInteger.incrementAndGet(), set.getValue());
            return set.getField() + " = ?";
        }).collect(Collectors.joining(",")));
        whereSQL(sb);
    }

    /**
     * 构建删除SQL语句
     * @param sb SQL语句构建器
     */
    private void deleteSQL(StringBuilder sb) {
        sb.append(statementType.getValue()).append(table);
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
        if (parse != null && parse.length() > 0)
            parse = SQLUtils.toSQLString(SQLUtils.parseStatements(parse, dbType), dbType);
        return this;
    }
}
