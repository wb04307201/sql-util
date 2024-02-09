package cn.wubo.sql.util;

import cn.wubo.sql.util.entity.EntityUtils;
import cn.wubo.sql.util.entity.TableModel;
import cn.wubo.sql.util.enums.GenerationType;
import cn.wubo.sql.util.enums.StatementCondition;
import cn.wubo.sql.util.exception.ModelSqlException;
import com.alibaba.druid.DbType;

import java.util.Objects;
import java.util.UUID;

/**
 * 根据实体类生成sql
 */
public class ModelSqlUtils {

    private ModelSqlUtils() {
    }

    /**
     * 根据数据插入SQL语句，根据标识的主键{@link @Key}生成主键值
     *
     * @param data 数据
     * @param <T>  数据类型
     * @return SQL对象
     */
    public static <T> SQL<T> insertSql(T data) {
        Class<T> clazz = (Class<T>) data.getClass();
        // 获取表信息
        TableModel tableModel = EntityUtils.getTable(clazz);
        SQL<T> sql = new SQL<T>(clazz) {
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
     * 生成更新SQL语句，根据标识的主键{@link @Key}更新数据
     * @param data 数据对象
     * @return SQL语句
     */
    public static <T> SQL<T> updateSql(T data) {
        Class<T> clazz = (Class<T>) data.getClass();
        // 获取表信息
        TableModel tableModel = EntityUtils.getTable(clazz);
        SQL<T> sql = new SQL<T>(clazz) {
        }.update();
        tableModel.getCols().stream().filter(col -> !col.getKey()).forEach(col -> {
            Object valObj = EntityUtils.getValue(col.getField(), data);
            if (valObj != null) sql.addSet(col.getColumnName(), valObj);
        });
        TableModel.ColumnModel keyCol = tableModel.getCols().stream().filter(TableModel.ColumnModel::getKey).findAny().orElseThrow(() -> new ModelSqlException("主键未定义"));
        sql.addWhereEQ(keyCol.getColumnName(), Objects.requireNonNull(EntityUtils.getValue(keyCol.getField(), data), "主键值不能为空"));
        return sql.parse();
    }


    /**
     * 构造删除SQL语句，根据标识的主键{@link @Key}删除数据
     * @param data 待删除的数据对象
     * @return SQL语句
     */
    public static <T> SQL<T> deleteSql(T data) {
        Class<T> clazz = (Class<T>) data.getClass();
        // 获取表信息
        TableModel tableModel = EntityUtils.getTable(clazz);
        SQL<T> sql = new SQL<T>(clazz) {
        }.delete();
        TableModel.ColumnModel keyCol = tableModel.getCols().stream().filter(TableModel.ColumnModel::getKey).findAny().orElseThrow(() -> new ModelSqlException("主键未定义"));
        sql.addWhereEQ(keyCol.getColumnName(), Objects.requireNonNull(EntityUtils.getValue(keyCol.getField(), data), "主键值不能为空"));
        return sql.parse();
    }


    /**
     * 生成分页查询SQL
     *
     * @param data   待查询的数据
     * @param dbType 数据库类型
     * @param offset 分页偏移量
     * @param count  分页大小
     * @return SQL对象
     */
    public static <T> SQL<T> selectByPageSql(T data, DbType dbType, Integer offset, Integer count) {
        Class<T> clazz = (Class<T>) data.getClass();
        // 获取表信息
        TableModel tableModel = EntityUtils.getTable(clazz);
        SQL<T> sql = new SQL<T>(clazz) {
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

        if (dbType != null) {
            sql.dialect(dbType);
            if (offset != null && count != null) {
                sql.page(offset, count);
            }
        }

        // 解析并返回SQL
        return sql.parse();
    }

    public static <T> SQL<T> selectSql(T data) {
        return selectByPageSql(data, null, null, null);
    }

    /**
     * 构造函数，用于创建一个SQL对象
     * @param data 数据对象
     * @return SQL对象
     */
    public static <T> SQL<T> SQL(T data) {
        return new SQL<T>((Class<T>) data.getClass()) {
        };
    }


    /**
     * 根据传入的数据类型，生成对应的SQL语句
     *
     * @return 生成的SQL语句列表
     */
    public static <T> SQL<T> createSql(T data) {
        return SQL(data).create().parse();
    }

    /**
     * 生成删除表的SQL语句
     *
     * @return 删除表的SQL语句
     */
    public static <T> SQL<T> dropSql(T data) {
        return SQL(data).drop().parse();
    }

}
