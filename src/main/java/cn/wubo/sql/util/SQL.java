package cn.wubo.sql.util;

import cn.wubo.sql.util.cache.MemoryCache;
import cn.wubo.sql.util.entity.EntityUtils;
import cn.wubo.sql.util.entity.TableModel;
import cn.wubo.sql.util.enums.DriverNameType;
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
import java.sql.SQLException;
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
     * 解析SQL语句。
     * 根据statementType的值，执行相应的SQL语句解析逻辑。
     *
     * @return SQL<T> 返回解析后的SQL对象。
     */
    public SQL<T> parse() {
        switch (statementType) { // 根据语句类型执行相应的解析方法
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
                // 如果没有匹配到任何类型，可能需要处理默认情况，此例中没有提供默认处理逻辑
        }
        return this; // 返回当前SQL对象实例，允许链式调用
    }

    /**
     * 构建SELECT语句。
     * 该方法根据提供的类信息（clazz），构建针对特定表的SELECT查询语句。查询语句的构建过程包括：
     * 1. 初始化StringBuilder；
     * 2. 获取表信息；
     * 3. 添加SELECT语句的类型；
     * 4. 根据columns列表添加选择的字段，如果列表为空，则默认选择所有字段；
     * 5. 添加FROM关键字和表名；
     * 6. 调用whereSQL方法添加WHERE条件；
     * 7. 根据dbType转换SQL语法；
     * 8. 如果存在分页信息，则对SQL应用分页限制；
     * 9. 将构建好的SQL语句添加到sqls列表中。
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
        // 根据dbType转换SQL字符串
        if (dbType != null) sql = SQLUtils.toSQLString(SQLUtils.parseStatements(sb.toString(), dbType), dbType);
        // 应用分页限制
        if (offset != null && count != null) sql = PagerUtils.limit(sql, dbType, offset, count);
        // 将构建好的SQL语句添加到集合中
        sqls.add(sql);
    }

    /**
     * 根据给定的where条件列表生成SQL WHERE子句。
     *
     * @param sb 用于追加生成的SQL WHERE子句的StringBuilder对象
     */
    private void whereSQL(StringBuilder sb) {
        // 将where条件列表转换为SQL语句字符串
        String whereSQL = wheres.stream().map(where -> {
            // 检查条件是否为OR条件
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
                // 处理BETWEEN和NOT BETWEEN条件
                return FunctionUtils.buildCondition(where, t -> FunctionUtils.compileConditionAnd(t, tt -> tt.getValue() instanceof List, tt -> ((List<?>) tt.getValue()).size() == 2), t -> {
                    List<?> valueObjs = (List<?>) where.getValue();
                    params.put(atomicInteger.incrementAndGet(), valueObjs.get(0));
                    params.put(atomicInteger.incrementAndGet(), valueObjs.get(1));
                    return where.getField() + where.getStatementCondition().getValue() + "? AND ?";
                });
            } else if (Boolean.TRUE.equals(FunctionUtils.compileConditionOr(where, t -> where.getStatementCondition() == StatementCondition.IN, t -> where.getStatementCondition() == StatementCondition.NOTIN))) {
                // 处理IN和NOT IN条件
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
                // 处理NULL和NOT NULL条件
                return where.getField() + where.getStatementCondition().getValue();
            } else {
                return null;
            }
        }).filter(Objects::nonNull).collect(Collectors.joining(" AND "));
        // 如果生成的SQL不为空，则追加到WHERE子句前
        if (!whereSQL.isEmpty()) sb.append(" WHERE ").append(whereSQL);
    }

    /**
     * 插入SQL语句的生成方法。
     * 该方法用于根据给定的实体类信息，生成插入数据的SQL语句。
     * 不接受任何参数。
     * 无返回值。
     */
    private void insertSQL() {
        StringBuilder sb = new StringBuilder();
        // 获取表信息，基于实体类 clazz
        TableModel tableModel = EntityUtils.getTable(clazz);
        // 构建插入语句的SQL字符串
        sb.append(statementType.getValue()).append(tableModel.getName()).append(" (").append(sets.stream().map(set -> {
            // 为参数映射表增加参数，并返回字段名
            params.put(atomicInteger.incrementAndGet(), set.getValue());
            return set.getField();
        }).collect(Collectors.joining(","))).append(") VALUES (").append(sets.stream().map(set -> "?").collect(Collectors.joining(","))).append(")");
        // 根据数据库类型，对SQL语句进行格式化处理
        if (dbType != null) {
            sqls.add(SQLUtils.toSQLString(SQLUtils.parseStatements(sb.toString(), dbType), dbType));
        } else {
            sqls.add(sb.toString());
        }
    }

    /**
     * 更新SQL语句
     * 该方法用于根据当前实体类的信息构建一个更新数据库记录的SQL语句。
     * 它首先会构建SQL语句的更新部分，然后追加WHERE条件，并最终生成完整的SQL字符串。
     * 生成的SQL语句会根据提供的数据库类型进行适配，并存储在sqls列表中。
     */
    private void updateSQL() {
        StringBuilder sb = new StringBuilder();
        // 获取表模型信息，用于构建SQL中的表名部分
        TableModel tableModel = EntityUtils.getTable(clazz);
        sb.append(statementType.getValue()).append(tableModel.getName()).append(" SET "); // 构建SQL语句的开始部分，包括UPDATE关键字和表名

        // 遍历所有的更新字段集合，构建SET部分的SQL语句，并为每个参数生成一个唯一的key存储在params映射中
        sb.append(sets.stream().map(set -> {
            params.put(atomicInteger.incrementAndGet(), set.getValue()); // 存储参数值
            return set.getField() + " = ?"; // 构建字段名和占位符的组合
        }).collect(Collectors.joining(","))); // 使用逗号将所有SET子句连接起来

        // 调用方法追加WHERE子句到SQL语句中
        whereSQL(sb);

        // 根据提供的数据库类型，对SQL语句进行适配处理，并添加到sqls列表中
        if (dbType != null) {
            sqls.add(SQLUtils.toSQLString(SQLUtils.parseStatements(sb.toString(), dbType), dbType));
        } else {
            sqls.add(sb.toString());
        }
    }

    /**
     * 构建删除SQL语句
     * 该方法用于根据当前实体类的信息，构建针对该实体类的删除操作的SQL语句。
     * SQL语句的构建过程包括：确定删除语句类型、指定表名、添加WHERE条件。
     * 注意：该方法不直接执行SQL语句，仅构造SQL字符串。
     */
    private void deleteSQL() {
        StringBuilder sb = new StringBuilder();
        // 获取当前实体类对应的表模型
        TableModel tableModel = EntityUtils.getTable(clazz);
        // 初始化SQL语句，添加删除语句类型和表名
        sb.append(statementType.getValue()).append(tableModel.getName());

        // 调用whereSQL方法，添加WHERE子句
        whereSQL(sb);
        // 根据数据库类型，将构建好的SQL语句转换为字符串或者解析为合适的SQL格式，并加入到SQL语句列表中
        if (dbType != null) sqls.add(SQLUtils.toSQLString(SQLUtils.parseStatements(sb.toString(), dbType), dbType));
        else sqls.add(sb.toString());
    }

    /**
     * 根据数据库类型创建对应的SQL建表语句。
     * 该方法会根据传入的类（clazz）信息和数据库类型（dbType），构建相应的SQL创建表语句。
     * 对于支持注释的数据库（如H2和PostgreSQL），还会添加表和列的注释。
     * 创建的SQL语句会存储在sqls列表中。
     */
    private void createSQL() {
        // 初始化StringBuilder用于构建SQL语句，和List用于存储额外的SQL语句（如列注释）
        StringBuilder sb = new StringBuilder().append("create table ");
        // 获取表的模型信息，包括表名、列信息和表描述
        TableModel tableModel = EntityUtils.getTable(clazz);
        if (dbType == DbType.h2 || dbType == DbType.postgresql) {
            // 如果是H2或PostgreSQL数据库，支持表和列的注释
            // 首先添加对表的注释
            sqls.add(String.format("comment on table %s is '%s'", tableModel.getName(), tableModel.getDesc()));
            sb.append(tableModel.getName()).append(" (");
            // 遍历列，为每一列添加定义和注释
            tableModel.getCols().forEach(col -> {
                sb.append(col.getColumnName()).append(" ").append(col.getDefinition()).append(",");
                // 添加对列的注释
                sqls.add(String.format("comment on column %s.%s is '%s'", tableModel.getName(), col.getColumnName(), col.getDesc()));
            });
            // 删除最后一个多余的逗号，然后关闭括号
            int length = sb.length();
            sb.delete(length - 1, length).append(")");
            // 将完整的建表语句添加到sqls列表的开头
            sqls.add(0, sb.toString());
        } else if (dbType == DbType.mysql) {
            // 如果是MySQL数据库，支持表注释，但在列定义中直接添加列注释
            sb.append(tableModel.getName()).append(" (");
            // 遍历列，为每一列添加定义和注释
            tableModel.getCols().forEach(col -> sb.append(col.getColumnName()).append(" ").append(col.getDefinition()).append(" null comment '").append(col.getDesc()).append("',"));
            // 删除最后一个多余的逗号，然后关闭括号，并添加表的总注释
            int length = sb.length();
            sb.delete(length - 1, length).append(")").append(" comment '").append(tableModel.getDesc()).append("'");
            // 将完整的建表语句添加到sqls列表的开头
            sqls.add(0, sb.toString());
        } else {
            // 对于其他不支持注释的数据库，只构建基本的建表语句
            sb.append(tableModel.getName()).append(" (");
            // 遍历列，添加每一列的定义
            tableModel.getCols().forEach(col -> sb.append(col.getColumnName()).append(" ").append(col.getDefinition()).append(","));
            // 删除最后一个多余的逗号，然后关闭括号
            int length = sb.length();
            sb.delete(length - 1, length).append(")");
            // 将完整的建表语句添加到sqls列表的开头
            sqls.add(0, sb.toString());
        }
    }

    /**
     * 添加删除表的SQL语句到sqls列表中。
     * 这个方法通过获取给定类对应的表模型，然后构造一个删除该表的SQL语句并添加到sqls列表中。
     */
    private void dropSQL() {
        // 获取表信息
        TableModel tableModel = EntityUtils.getTable(clazz);
        // 构造并添加删除表的SQL语句
        sqls.add("DROP TABLE " + tableModel.getName());
    }

    /**
     * 设置数据库方言
     * 此方法用于根据关联的实体类设置操作数据库时使用的方言。
     * 方言的设置依赖于实体类所映射的数据库表和数据源。
     *
     * @return SQL对象  返回当前SQL对象实例，支持链式调用。
     */
    private SQL<T> dialect() {
        if (this.clazz != null) {
            // 根据实体类获取对应的表模型
            TableModel tableModel = EntityUtils.getTable(this.clazz);
            // 从表模型的数据源URL中解析出数据库类型
            this.dbType = DriverNameType.getDriverNameTypeFromUrl(tableModel.getDs().getUrl());
        }
        return this;
    }

    /**
     * 设置数据库方言
     * 本方法用于根据不同的数据库类型，设置相应的数据库方言。方言的设置将影响SQL语句的生成，确保生成的SQL语句能够与特定的数据库正确交互。
     *
     * @param dbType 数据库方言类型，指示当前使用的数据库类型，例如MySQL、Oracle等。
     * @return 返回SQL对象本身，支持链式调用。这样可以在调用dialect方法后继续调用其他方法，而不需要显式地保存或重新获取SQL对象的引用。
     */
    public SQL<T> dialect(DbType dbType) {
        this.dbType = dbType; // 设置当前SQL对象的数据库类型为传入的dbType参数
        return this; // 返回当前SQL对象，允许链式调用
    }

    /**
     * 获取数据库类型
     * <p>
     * 通过分析数据库连接对象获取数据库的类型。此方法会尝试从数据库连接的元数据中提取驱动名称，
     * 并据此判断数据库的类型。
     *
     * @param conn 数据库连接对象，用于获取数据库的元数据信息。
     */
    public void dialect(Connection conn) {
        try {
            // 从数据库连接的元数据中提取驱动名称，进而判断数据库类型
            this.dbType = DriverNameType.getDriverNameTypeFromMeta(conn.getMetaData().getDriverName());
        } catch (SQLException e) {
            // 如果在获取元数据过程中出现SQLException，将其转换为SQLRuntimeException并抛出
            throw new SQLRuntimeException(e);
        }
    }

    /**
     * 分页查询
     * 该方法用于设置查询的分页信息，根据提供的偏移量和每页显示的数量来设定。
     *
     * @param offset 分页偏移量，表示从结果集的第几个位置开始取。
     * @param count  每页显示数量，表示每页最多显示多少条记录。
     * @return SQL对象，返回当前SQL实例，支持链式调用。
     * @throws SQLRuntimeException 如果在设置分页前未使用dialect设置数据库类型，或者在非查询模式下调用此方法，将抛出异常。
     */
    public SQL<T> page(int offset, int count) {
        // 检查数据库类型是否已设置
        if (dbType == null) dialect();
        if (dbType == null) throw new SQLRuntimeException("设置分页前，请使用dialect设置数据库类型!");
        if (statementType != StatementType.SELECT) throw new SQLRuntimeException("非查询模式，不能调用page方法！");
        this.offset = offset;
        this.count = count;
        return this;
    }

    /**
     * 执行查询操作。该方法用于执行数据库查询语句，并返回查询结果的列表。
     *
     * @param connection 数据库连接对象，用于与数据库建立连接。
     * @return 查询结果的列表，类型为泛型T。
     * @throws SQLRuntimeException 如果当前操作非查询模式，即statementType不为SELECT，抛出此异常。
     */
    public List<T> executeQuery(Connection connection) {
        // 检查当前操作是否为查询模式
        if (statementType != StatementType.SELECT)
            throw new SQLRuntimeException("非查询模式，不能调用executeQuery方法！");

        // 初始化数据库类型，如果还未设置
        if (dbType == null) dialect();
        if (dbType == null) dialect(connection);

        // 解析查询语句及相关参数
        parse();

        // 执行查询操作，并返回查询结果
        return ExecuteSqlUtils.executeQuery(connection, this.sqls.get(0), this.params, this.clazz);
    }

    /**
     * 执行更新操作，主要用于插入、更新和删除数据库记录。
     *
     * @param connection 数据库连接对象，用于执行更新操作时的数据库连接。
     * @return 返回更新操作所影响的行数。
     * @throws SQLRuntimeException 如果当前操作模式不是新增、更新、删除，则抛出异常。
     */
    public int executeUpdate(Connection connection) {
        // 检查操作类型是否为新增、更新或删除，否则抛出异常
        if (statementType != StatementType.INSERT && statementType != StatementType.UPDATE && statementType != StatementType.DELETE)
            throw new SQLRuntimeException("非新增、更新、删除模式，不能调用executeUpdate方法！");

        // 如果数据库类型未设置，则自动识别数据库类型
        if (dbType == null) dialect();
        // 如果仍未设置数据库类型，则通过连接自动识别
        if (dbType == null) dialect(connection);

        // 解析SQL语句
        parse();

        // 执行更新操作，并返回影响的行数
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
        String tableName = tableModel.getName();
        if (dbType == null) dialect();
        if (dbType == null) dialect(connection);
        if (dbType.equals(DbType.h2) || dbType.equals(DbType.dm)) tableName = tableName.toUpperCase();
        else tableName = tableName.toLowerCase();
        return ExecuteSqlUtils.isTableExists(connection, tableName);
    }

    /**
     * 创建表
     *
     * @param connection 数据库连接
     * @return 创建成功返回0，否则返回-1
     */
    public int createTable(Connection connection) {
        if (statementType != StatementType.CREATE) throw new SQLRuntimeException("非建表模式，不能调用createTable方法！");
        if (dbType == null) dialect();
        if (dbType == null) dialect(connection);
        parse();
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
        if (dbType == null) dialect();
        if (dbType == null) dialect(connection);
        parse();
        return ExecuteSqlUtils.executeUpdate(connection, this.sqls.get(0));
    }
}
