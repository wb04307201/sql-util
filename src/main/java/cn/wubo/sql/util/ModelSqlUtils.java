package cn.wubo.sql.util;

import cn.wubo.sql.util.entity.EntityUtils;
import cn.wubo.sql.util.entity.TableModel;
import cn.wubo.sql.util.enums.GenerationType;
import cn.wubo.sql.util.enums.StatementCondition;
import cn.wubo.sql.util.exception.ModelSqlUtilsException;

import java.util.ArrayList;
import java.util.List;
import java.util.Objects;
import java.util.UUID;

/**
 * 根据实体类生成sql
 */
public class ModelSqlUtils {

    private ModelSqlUtils() {
    }

    /**
     * 根据数据插入SQL语句
     *
     * @param data 数据
     * @param <T>  数据类型
     * @return SQL对象
     */
    public static <T> SQL<T> insertSql(T data) {
        Class<T> clazz = (Class<T>) data.getClass();
        // 检查表是否存在，如果不存在则添加表信息
        if (Boolean.FALSE.equals(EntityUtils.check(clazz.getName()))) EntityUtils.putTable(clazz);

        // 获取表信息
        TableModel tableModel = EntityUtils.getTable(clazz.getName());
        SQL<T> sql = new SQL<T>().insert();

        // 遍历字段列表，将非空字段添加到SQL的set语句中
        tableModel.getCols().stream().forEach(col -> {
            if (Boolean.TRUE.equals(col.getKey()) && col.getGenerationType() == GenerationType.UUID) {
                sql.addSet(col.getColumnName(), UUID.randomUUID().toString());
            } else {
                Object valObj = EntityUtils.getValue(col.getField(), data);
                if (valObj != null) sql.addSet(col.getColumnName(), valObj);
            }
        });

        // 解析SQL语句
        return sql.parse();
    }

    /**
     * 生成更新SQL语句
     *
     * @param data 数据对象
     * @return SQL对象
     */
    public static <T> SQL<T> updateSql(T data) {
        Class<T> clazz = (Class<T>) data.getClass();
        // 检查表是否存在，如果不存在则添加表信息
        if (Boolean.FALSE.equals(EntityUtils.check(clazz.getName()))) EntityUtils.putTable(clazz);

        // 获取表信息
        TableModel tableModel = EntityUtils.getTable(clazz.getName());

        // 创建SQL对象并设置表名
        SQL<T> sql = new SQL<T>().update();

        // 遍历所有字段，将非id字段添加到set语句中
        tableModel.getCols().stream().filter(col -> !col.getKey()).forEach(col -> {
            Object valObj = EntityUtils.getValue(col.getField(), data);
            if (valObj != null) sql.addSet(col.getColumnName(), valObj);
        });

        // 获取id字段
        TableModel.ColumnModel keyCol = tableModel.getCols().stream().filter(TableModel.ColumnModel::getKey).findAny().orElseThrow(() -> new ModelSqlUtilsException("主键未定义"));

        // 添加where语句
        sql.addWhereEQ(keyCol.getColumnName(), Objects.requireNonNull(EntityUtils.getValue(keyCol.getField(), data), "主键值不能为空"));

        return sql.parse();
    }

    /**
     * 构造删除SQL语句
     *
     * @param data 待删除的数据对象
     * @return 构造好的SQL语句
     */
    public static <T> SQL<T> deleteSql(T data) {
        Class<T> clazz = (Class<T>) data.getClass();
        // 检查表是否存在，如果不存在则添加表信息
        if (Boolean.FALSE.equals(EntityUtils.check(clazz.getName()))) EntityUtils.putTable(clazz);

        // 获取表信息
        TableModel tableModel = EntityUtils.getTable(clazz.getName());
        SQL<T> sql = new SQL<T>().delete();

        // 根据id字段名筛选出id字段
        TableModel.ColumnModel keyCol = tableModel.getCols().stream().filter(TableModel.ColumnModel::getKey).findAny().orElseThrow(() -> new ModelSqlUtilsException("主键未定义"));

        // 在sql语句中加入where条件，根据id值进行等值比较
        sql.addWhereEQ(keyCol.getColumnName(), Objects.requireNonNull(EntityUtils.getValue(keyCol.getField(), data), "主键值不能为空"));

        // 解析并返回SQL
        return sql.parse();
    }

    /**
     * 生成查询的SQL语句
     *
     * @param tableName 表名
     * @param data      数据
     * @param <T>       实体类
     * @return SQL对象
     */
    public static <T> SQL<T> selectSql(String tableName, T data) {
        Class<T> clazz = (Class<T>) data.getClass();
        // 检查表是否存在，如果不存在则添加表信息
        if (Boolean.FALSE.equals(EntityUtils.check(clazz.getName()))) EntityUtils.putTable(clazz);

        // 获取表信息
        TableModel tableModel = EntityUtils.getTable(clazz.getName());
        SQL<T> sql = new SQL<T>().select();

        // 遍历字段列表，根据字段值生成where条件
        tableModel.getCols().forEach(col -> {
            Object valObj = EntityUtils.getValue(col.getField(), data);
            if (valObj != null) {
                if (col.getStatementCondition() == StatementCondition.EQ) sql.addWhereEQ(col.getColumnName(), valObj);
                else if (col.getStatementCondition() == StatementCondition.UEQ)
                    sql.addWhereUEQ(col.getColumnName(), valObj);
                else if (col.getStatementCondition() == StatementCondition.LIKE)
                    sql.addWhereLIKE(col.getColumnName(), valObj);
                else if (col.getStatementCondition() == StatementCondition.LLIKE)
                    sql.addWhereLLIKE(col.getColumnName(), valObj);
                else if (col.getStatementCondition() == StatementCondition.RLIKE)
                    sql.addWhereRLIKE(col.getColumnName(), valObj);
                else if (col.getStatementCondition() == StatementCondition.GT)
                    sql.addWhereGT(col.getColumnName(), valObj);
                else if (col.getStatementCondition() == StatementCondition.LT)
                    sql.addWhereLT(col.getColumnName(), valObj);
                else if (col.getStatementCondition() == StatementCondition.GTEQ)
                    sql.addWhereGTEQ(col.getColumnName(), valObj);
                else if (col.getStatementCondition() == StatementCondition.LTEQ)
                    sql.addWhereLTEQ(col.getColumnName(), valObj);
            }
        });

        // 解析并返回SQL
        return sql.parse();
    }

    /**
     * 根据给定的实体类生成创建表的SQL语句
     *
     * @param clazz 实体类的Class对象
     * @return 包含创建表的SQL语句的List
     */
    public static <T> List<String> createSql(Class<T> clazz) {
        // 初始化StringBuilder和List
        StringBuilder sb = new StringBuilder();
        List<String> sqls = new ArrayList<>();

        // 检查表是否存在，如果不存在则添加表信息
        if (Boolean.FALSE.equals(EntityUtils.check(clazz.getName()))) EntityUtils.putTable(clazz);

        // 获取表信息
        TableModel tableModel = EntityUtils.getTable(clazz.getName());

        // 添加表注释
        sqls.add(String.format("comment on table %s is '%s'", tableModel.getName(), tableModel.getDesc()));

        // 构建创建表的SQL语句
        sb.append("create table ").append(tableModel.getName()).append(" (");
        tableModel.getCols().forEach(col -> {
            sb.append(col.getFieldName()).append(" ").append(col.getDefinition()).append(",");
            // 添加列注释
            sqls.add(String.format("comment on column %s.%s is '%s'", tableModel.getName(), col.getColumnName(), col.getDesc()));
        });

        // 删除最后一个逗号并添加表结束符号
        int length = sb.length();
        sb.delete(length - 1, length).append(")");

        // 添加创建表的SQL语句到List中
        sqls.add(0, sb.toString());

        // 返回创建表的SQL语句
        return sqls;
    }

    /**
     * 根据传入的数据类型，生成对应的SQL语句
     *
     * @param data 传入的数据对象
     * @return 生成的SQL语句列表
     */
    public static <T> List<String> createSql(T data) {
        return createSql(data.getClass());
    }

    /**
     * 生成删除表的SQL语句
     *
     * @param clazz 数据表对应的实体类
     * @return 删除表的SQL语句
     */
    public static <T> String dropSql(Class<T> clazz) {
        // 检查数据表是否存在
        if (Boolean.FALSE.equals(EntityUtils.check(clazz.getName()))) {
            // 将数据表添加到实体类映射表中
            EntityUtils.putTable(clazz);
        }
        // 获取数据表的模型对象
        TableModel tableModel = EntityUtils.getTable(clazz.getName());
        // 返回删除表的SQL语句
        return "DROP TABLE " + tableModel.getName();
    }

}
