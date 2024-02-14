package cn.wubo.sql.util;

import cn.wubo.sql.util.cache.MemoryCache;
import cn.wubo.sql.util.entity.EntityUtils;
import cn.wubo.sql.util.entity.TableModel;
import cn.wubo.sql.util.enums.StatementCondition;
import cn.wubo.sql.util.enums.StatementType;
import cn.wubo.sql.util.exception.SQLRuntimeException;
import cn.wubo.sql.util.utils.FunctionUtils;
import cn.wubo.sql.util.utils.MapUtils;
import com.alibaba.druid.DbType;
import com.alibaba.druid.sql.PagerUtils;
import com.alibaba.druid.sql.SQLUtils;
import lombok.AllArgsConstructor;
import lombok.Data;

import java.lang.reflect.ParameterizedType;
import java.lang.reflect.Type;
import java.sql.Connection;
import java.util.*;
import java.util.concurrent.atomic.AtomicInteger;
import java.util.stream.Collectors;

public class SQL<T> {
    private Class<T> clazz;
    private Integer offset;
    private Integer count;
    private StatementType statementType;
    private List<String> columns = new ArrayList<>();
    private List<Set> sets = new ArrayList<>();
    private List<Where> wheres = new ArrayList<>();
    private AtomicInteger atomicInteger;
    private DbType dbType;
    private List<String> sqls;
    private Map<Integer, Object> params = new HashMap<>();

    public SQL() {
        Type superClass = this.getClass().getGenericSuperclass();
        try {
            ParameterizedType parameterizedType = (ParameterizedType) superClass;
            Type tempType = parameterizedType.getActualTypeArguments()[0];
            Class<T> tempClazz = (Class<T>) MemoryCache.getClass(tempType);
            if (tempClazz == null) {
                if (tempType.toString().startsWith("class")) tempClazz = (Class<T>) tempType;
                else tempClazz = (Class<T>) ((ParameterizedType) tempType).getRawType();
                if (Boolean.TRUE.equals(MapUtils.isMap(tempClazz))) throw new SQLRuntimeException("SQL不支持Map!");
                MemoryCache.putClass(tempType, tempClazz);
                tempClazz = (Class<T>) MemoryCache.getClass(tempType);
            }
            this.clazz = tempClazz;
        } catch (Exception e) {
            throw new SQLRuntimeException("请使用new SQL<T>(){}方式声明！");
        }
    }

    public SQL(Class<T> clazz) {
        this.clazz = clazz;
    }

    /**
     * 设置SQL语句类型为查询（SELECT）。
     *
     * @param columns 查询的列名
     * @return SQL对象本身
     */
    public SQL<T> select(String... columns) {
        this.statementType = StatementType.SELECT;
        this.columns = Arrays.asList(columns);
        this.sets = new ArrayList<>();
        this.wheres = new ArrayList<>();
        this.params = new HashMap<>();
        this.sqls = new ArrayList<>();
        this.offset = null;
        this.count = null;
        return this;
    }

    /**
     * 执行查询操作
     *
     * @return 返回SQL对象
     */
    public SQL<T> select() {
        return select("*");
    }


    /**
     * 设置SQL语句类型为插入（INSERT）。
     *
     * @return SQL对象本身
     */
    public SQL<T> insert() {
        this.statementType = StatementType.INSERT;
        this.sets = new ArrayList<>();
        this.wheres = new ArrayList<>();
        this.params = new HashMap<>();
        this.sqls = new ArrayList<>();
        this.offset = null;
        this.count = null;
        return this;
    }


    /**
     * 设置SQL语句类型为更新（UPDATE）。
     *
     * @return SQL对象本身
     */
    public SQL<T> update() {
        this.statementType = StatementType.UPDATE;
        this.sets = new ArrayList<>();
        this.wheres = new ArrayList<>();
        this.params = new HashMap<>();
        this.sqls = new ArrayList<>();
        this.offset = null;
        this.count = null;
        return this;
    }

    /**
     * 设置SQL语句类型为删除（DELETE）。
     *
     * @return SQL对象本身
     */
    public SQL<T> delete() {
        this.statementType = StatementType.DELETE;
        this.sets = new ArrayList<>();
        this.wheres = new ArrayList<>();
        this.sqls = new ArrayList<>();
        this.params = new HashMap<>();
        this.offset = null;
        this.count = null;
        return this;
    }

    /**
     * 执行创建操作
     *
     * @return 返回SQL对象
     */
    public SQL<T> create() {
        this.statementType = StatementType.CREATE;
        this.sets = new ArrayList<>();
        this.wheres = new ArrayList<>();
        this.sqls = new ArrayList<>();
        this.params = new HashMap<>();
        this.offset = null;
        this.count = null;
        return this;
    }

    /**
     * 执行删除操作
     *
     * @return 返回SQL对象
     */
    public SQL<T> drop() {
        this.statementType = StatementType.DROP;
        this.sets = new ArrayList<>();
        this.wheres = new ArrayList<>();
        this.sqls = new ArrayList<>();
        this.params = new HashMap<>();
        this.offset = null;
        this.count = null;
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
        // 添加一个设置对象到集合中
        sets.add(new Set(field, value));
        // 返回当前SQL对象
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
     *
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
     *
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
     *
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
     *
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
     *
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
     *
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
     *
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
     *
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
     *
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
     *
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
     *
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
     *
     * @param field 字段名
     * @return SQL对象
     */
    public SQL<T> addWhereNULL(String field) {
        wheres.add(new Where(field, StatementCondition.NULL));
        return this;
    }

    /**
     * 添加一个WHERE NOT NULL条件
     *
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

    /**
     * 解析SQL语句
     * @return SQL对象
     */
    public SQL<T> parse() {
        if (this.clazz != null) {
            TableModel tableModel = EntityUtils.getTable(this.clazz);
            transDbtype(tableModel.getDs().getUrl());
        }
        atomicInteger = new AtomicInteger(0);
        switch (statementType) {
            case SELECT:
                selectSQL();
                break;
            case INSERT:
                insertSQL();
                break;
            case UPDATE:
                updateSQL();
                break;
            case DELETE:
                deleteSQL();
                break;
            case CREATE:
                createSQL();
                break;
            case DROP:
                dropSQL();
                break;
            default:
        }
        return this;
    }

    /**
     * 根据数据库连接URL判断数据库类型
     * @param url 数据库连接URL
     */
    private void transDbtype(String url) {
        if (url != null && !url.isEmpty()) {
            if (url.startsWith("jdbc:h2:")) dbType = DbType.h2; // 如果URL以jdbc:h2:开头，则数据库类型为h2
            else if (url.startsWith("jdbc:mysql:")) dbType = DbType.mysql; // 如果URL以jdbc:mysql:开头，则数据库类型为mysql
            else if (url.startsWith("jdbc:postgresql:")) dbType = DbType.postgresql; // 如果URL以jdbc:postgresql:开头，则数据库类型为postgresql
        }
    }

    /**
     * 构建SELECT语句
     */
    private void selectSQL() {
        StringBuilder sb = new StringBuilder();
        // 获取表信息
        TableModel tableModel = EntityUtils.getTable(clazz);
        // 添加SELECT语句的类型
        sb.append(statementType.getValue());
        // 如果columns不为空，则遍历columns列表，将每个字段名添加到sb中，并在每个字段名后面添加逗号
        if (!columns.isEmpty()) columns.forEach(str -> sb.append(str).append(","));
            // 如果columns为空，则添加"*"和逗号
        else sb.append("*,");
        // 删除最后一个逗号，并添加FROM和table
        sb.delete(sb.length() - 1, sb.length()).append(" FROM ").append(tableModel.getName());
        // 添加WHERE语句
        whereSQL(sb);
        String sql = sb.toString();
        if (dbType != null) sql = SQLUtils.toSQLString(SQLUtils.parseStatements(sb.toString(), dbType), dbType);
        if (offset != null && count != null) sql = PagerUtils.limit(sql, dbType, offset, count);
        sqls.add(sql);
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
     * Insert SQL
     */
    private void insertSQL() {
        StringBuilder sb = new StringBuilder();
        // 获取表信息
        TableModel tableModel = EntityUtils.getTable(clazz);
        sb.append(statementType.getValue()).append(tableModel.getName()).append(" (").append(sets.stream().map(set -> {
            params.put(atomicInteger.incrementAndGet(), set.getValue());
            return set.getField();
        }).collect(Collectors.joining(","))).append(") VALUES (").append(sets.stream().map(set -> "?").collect(Collectors.joining(","))).append(")");
        if (dbType != null) {
            sqls.add(SQLUtils.toSQLString(SQLUtils.parseStatements(sb.toString(), dbType), dbType));
        } else {
            sqls.add(sb.toString());
        }
    }

    /**
     * Update SQL
     */
    private void updateSQL() {
        StringBuilder sb = new StringBuilder();
        // 获取表信息
        TableModel tableModel = EntityUtils.getTable(clazz);
        sb.append(statementType.getValue()).append(tableModel.getName()).append(" SET "); // Append the UPDATE statement with the table name
        sb.append(sets.stream().map(set -> { // Iterate over each set
            params.put(atomicInteger.incrementAndGet(), set.getValue()); // Add the set value to the params map with a unique key
            return set.getField() + " = ?"; // Append the set field with a placeholder for the value
        }).collect(Collectors.joining(","))); // Join the set fields with commas
        whereSQL(sb); // Append the WHERE clause to the SQL statement
        if (dbType != null) {
            sqls.add(SQLUtils.toSQLString(SQLUtils.parseStatements(sb.toString(), dbType), dbType));
        } else {
            sqls.add(sb.toString());
        }
    }

    /**
     * 构建删除SQL语句
     */
    private void deleteSQL() {
        StringBuilder sb = new StringBuilder();
        // 获取表信息
        TableModel tableModel = EntityUtils.getTable(clazz);
        // 添加删除语句类型和表名
        sb.append(statementType.getValue()).append(tableModel.getName());

        // 添加WHERE子句
        whereSQL(sb);
        if (dbType != null) sqls.add(SQLUtils.toSQLString(SQLUtils.parseStatements(sb.toString(), dbType), dbType));
        else sqls.add(sb.toString());
    }

    /**
     * 创建SQL语句
     */
    private void createSQL() {
        // 初始化StringBuilder和List
        StringBuilder sb = new StringBuilder().append("create table ");
        // 获取表信息
        TableModel tableModel = EntityUtils.getTable(clazz);
        if (dbType == DbType.h2 || dbType == DbType.postgresql || dbType == DbType.oracle) {
            // 添加表注释
            sqls.add(String.format("comment on table %s is '%s'", tableModel.getName(), tableModel.getDesc()));
            sb.append(tableModel.getName()).append(" (");
            tableModel.getCols().forEach(col -> {
                sb.append(col.getColumnName()).append(" ").append(col.getDefinition()).append(",");
                // 添加列注释
                sqls.add(String.format("comment on column %s.%s is '%s'", tableModel.getName(), col.getColumnName(), col.getDesc()));
            });
            int length = sb.length();
            sb.delete(length - 1, length).append(")");
            sqls.add(0, sb.toString());
        } else if (dbType == DbType.mysql) {
            sb.append(tableModel.getName()).append(" (");
            tableModel.getCols().forEach(col -> sb.append(col.getColumnName()).append(" ").append(col.getDefinition()).append(" null comment '").append(col.getDesc()).append("',"));
            int length = sb.length();
            sb.delete(length - 1, length).append(")").append(" comment '").append(tableModel.getDesc()).append("'");
            sqls.add(0, sb.toString());
        } else {
            sb.append(tableModel.getName()).append(" (");
            tableModel.getCols().forEach(col -> sb.append(col.getColumnName()).append(" ").append(col.getDefinition()).append(","));
            int length = sb.length();
            sb.delete(length - 1, length).append(")");
            sqls.add(0, sb.toString());
        }
    }

    /**
     * 添加删除表的SQL语句
     */
    private void dropSQL() {
        // 获取表信息
        TableModel tableModel = EntityUtils.getTable(clazz);
        sqls.add("DROP TABLE " + tableModel.getName());
    }

    /**
     * 设置数据库方言
     *
     * @param dbType 数据库方言
     * @return SQL对象
     */
    public SQL<T> dialect(DbType dbType) {
        this.dbType = dbType;
        return this;
    }

    /**
     * 分页查询
     *
     * @param offset 分页偏移量
     * @param count  每页显示数量
     * @return SQL对象
     */
    public SQL<T> page(int offset, int count) {
        // 检查数据库类型是否已设置
        if (dbType == null) throw new SQLRuntimeException("设置分页前，请使用dialect设置数据库类型!");
        if (statementType != StatementType.SELECT) throw new SQLRuntimeException("非查询模式，不能调用page方法！");
        this.offset = offset;
        this.count = count;
        return this;
    }

    /**
     * 执行查询操作
     *
     * @param connection 数据库连接
     * @return 查询结果列表
     */
    public List<T> executeQuery(Connection connection) {
        if (statementType != StatementType.SELECT)
            throw new SQLRuntimeException("非查询模式，不能调用executeQuery方法！");
        return ExecuteSqlUtils.executeQuery(connection, this.sqls.get(0), this.params, this.clazz);
    }

    /**
     * 执行更新操作
     *
     * @param connection 数据库连接
     * @return 更新的行数
     * @throws SQLRuntimeException 如果不是新增、更新、删除模式，抛出异常
     */
    public int executeUpdate(Connection connection) {
        // 检查是否是新增、更新、删除模式
        if (statementType != StatementType.INSERT && statementType != StatementType.UPDATE && statementType != StatementType.DELETE)
            throw new SQLRuntimeException("非新增、更新、删除模式，不能调用executeUpdate方法！");

        // 执行更新操作
        return ExecuteSqlUtils.executeUpdate(connection, this.sqls.get(0), this.params);
    }

    /**
     * 判断表是否存在
     *
     * @param connection 数据库连接
     * @return 存在返回true，否则返回false
     */
    public Boolean isTableExists(Connection connection) {
        // 获取表信息
        TableModel tableModel = EntityUtils.getTable(clazz);
        return ExecuteSqlUtils.isTableExists(connection, tableModel.getName());
    }

    /**
     * 创建表
     *
     * @param connection 数据库连接
     * @return 创建成功返回0，否则返回-1
     */
    public int createTable(Connection connection) {
        if (statementType != StatementType.CREATE) throw new SQLRuntimeException("非建表模式，不能调用createTable方法！");
        return ExecuteSqlUtils.executeUpdate(connection, this.sqls);
    }

    /**
     * 删除表
     *
     * @param connection 数据库连接
     * @return 删除成功返回0，否则返回-1
     */
    public int dropTable(Connection connection) {
        if (statementType != StatementType.DROP) throw new SQLRuntimeException("非删表模式，不能调用dropTable方法！");
        return ExecuteSqlUtils.executeUpdate(connection, this.sqls.get(0));
    }

}
