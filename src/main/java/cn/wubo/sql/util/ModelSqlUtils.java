package cn.wubo.sql.util;

import cn.wubo.sql.util.entity.EntityUtils;
import cn.wubo.sql.util.entity.TableModel;
import cn.wubo.sql.util.enums.GenerationType;
import cn.wubo.sql.util.enums.StatementCondition;
import cn.wubo.sql.util.exception.ModelSqlException;

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
        // 获取表信息
        TableModel tableModel = EntityUtils.getTable(clazz);
        SQL<T> sql = new SQL<T>() {
        }.insert();

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
        // 获取表信息
        TableModel tableModel = EntityUtils.getTable(clazz);

        // 创建SQL对象并设置表名
        SQL<T> sql = new SQL<T>() {
        }.update();

        // 遍历所有字段，将非id字段添加到set语句中
        tableModel.getCols().stream().filter(col -> !col.getKey()).forEach(col -> {
            Object valObj = EntityUtils.getValue(col.getField(), data);
            if (valObj != null) sql.addSet(col.getColumnName(), valObj);
        });

        // 获取id字段
        TableModel.ColumnModel keyCol = tableModel.getCols().stream().filter(TableModel.ColumnModel::getKey).findAny().orElseThrow(() -> new ModelSqlException("主键未定义"));

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
        // 获取表信息
        TableModel tableModel = EntityUtils.getTable(clazz);
        SQL<T> sql = new SQL<T>() {
        }.delete();

        // 根据id字段名筛选出id字段
        TableModel.ColumnModel keyCol = tableModel.getCols().stream().filter(TableModel.ColumnModel::getKey).findAny().orElseThrow(() -> new ModelSqlException("主键未定义"));

        // 在sql语句中加入where条件，根据id值进行等值比较
        sql.addWhereEQ(keyCol.getColumnName(), Objects.requireNonNull(EntityUtils.getValue(keyCol.getField(), data), "主键值不能为空"));

        // 解析并返回SQL
        return sql.parse();
    }

    /**
     * 生成查询的SQL语句
     *
     * @param data 数据
     * @param <T>  实体类
     * @return SQL对象
     */
    public static <T> SQL<T> selectSql(T data) {
        Class<T> clazz = (Class<T>) data.getClass();
        // 获取表信息
        TableModel tableModel = EntityUtils.getTable(clazz);
        SQL<T> sql = new SQL<T>() {
        }.select();

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
     * 根据传入的数据类型，生成对应的SQL语句
     *
     * @return 生成的SQL语句列表
     */
    public static <T> SQL<T> createSql() {
        return new SQL<T>() {
        }.create();
    }

    /**
     * 生成删除表的SQL语句
     *
     * @return 删除表的SQL语句
     */
    public static <T> SQL<T> dropSql() {
        return new SQL<T>() {
        }.drop();
    }

}
