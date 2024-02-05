package cn.wubo.sql.util;

import cn.wubo.sql.util.exception.ModelSqlUtilsException;

import java.lang.reflect.Field;
import java.lang.reflect.Modifier;
import java.math.BigDecimal;
import java.sql.Blob;
import java.sql.Clob;
import java.sql.Time;
import java.sql.Timestamp;
import java.text.SimpleDateFormat;
import java.util.*;
import java.util.stream.Collectors;

/**
 * 根据实体类生成sql
 */
public class ModelSqlUtils {

    private ModelSqlUtils() {
    }

    /**
     * 反射获取类和父类的字段
     * 排除合成字段
     * 排除static final
     *
     * @param clazz  类
     * @param fields Field集合
     */
    private static void getFields(Class<?> clazz, List<Field> fields) {
        if (clazz != null) {
            // 获取类的字段
            fields.addAll(Arrays.stream(clazz.getDeclaredFields()).filter(field -> !field.isSynthetic()).filter(field -> !(Modifier.isFinal(field.getModifiers()) && Modifier.isStatic(field.getModifiers()))).collect(Collectors.toList()));

            // 获取父类的字段
            getFields(clazz.getSuperclass(), fields);
        }
    }


    /**
     * 对值的处理
     *
     * @param field 字段
     * @param data  数据
     * @param <T>   数据类型
     * @return 处理后的值
     */
    private static <T> Object getValue(Field field, T data) {
        try {
            field.setAccessible(true);
            Object obj = field.get(data);
            if (obj != null) {
                // 如果值是java.sql.Timestamp类型，则格式化为"yyyy-MM-dd HH:mm:ss.SSS"格式
                if (obj instanceof java.sql.Timestamp)
                    return new SimpleDateFormat("yyyy-MM-dd HH:mm:ss.SSS").format(obj);
                // 如果值是Date类型，则格式化为"yyyy-MM-dd"格式
                else if (obj instanceof Date) return new SimpleDateFormat("yyyy-MM-dd").format(obj);
                // 其他情况下直接返回值
                else return obj;
            } else {
                return null;
            }
        } catch (IllegalAccessException e) {
            throw new ModelSqlUtilsException(e);
        }
    }


    /**
     * 转换数据库类型
     *
     * @param field 要转换类型的字段
     * @return 转换后的数据库类型
     */
    private static String getType(Field field) {
        // 根据字段类型进行数据库类型的转换
        if (field.getType().equals(Integer.class)) {
            return "int";  // 整数类型转换为数据库的整数类型
        } else if (field.getType().equals(Long.class)) {
            return "bigint";  // 整数类型转换为数据库的大整数类型
        } else if (field.getType().equals(Float.class)) {
            return "float";  // 浮点数类型转换为数据库的浮点数类型
        } else if (field.getType().equals(Double.class)) {
            return "double";  // 浮点数类型转换为数据库的双精度浮点数类型
        } else if (field.getType().equals(BigDecimal.class)) {
            return "numeric";  // 使用BigDecimal类型的字段转换为数据库的数字类型
        } else if (field.getType().equals(java.util.Date.class) || field.getType().equals(java.sql.Date.class)) {
            return "date";  // 日期类型转换为数据库的日期类型
        } else if (field.getType().equals(Time.class)) {
            return "time";  // 时间类型转换为数据库的时间类型
        } else if (field.getType().equals(Timestamp.class) || field.getType().equals(Calendar.class)) {
            return "timestamp";  // 时间戳类型或日历类型转换为数据库的时间戳类型
        } else if (field.getType().equals(Boolean.class)) {
            return "bit";  // 布尔类型转换为数据库的位类型
        } else if (field.getType().equals(Blob.class)) {
            return "blob";  // BLOB类型转换为数据库的BLOB类型
        } else if (field.getType().equals(Clob.class)) {
            return "clob";  // CLOB类型转换为数据库的CLOB类型
        } else {
            return "varchar2";  // 默认转换为数据库的VARCHAR2类型
        }
    }


    /**
     * 生成插入SQL语句
     *
     * @param tableName 表名
     * @param data      数据
     * @param <T>       实体类
     * @return SQL对象
     */
    public static <T> SQL<T> insertSql(String tableName, T data) {
        // 创建SQL对象并设置表名
        SQL<T> sql = SQL.<T>insert().table(tableName);

        // 获取数据类的字段
        List<Field> fields = new ArrayList<>();
        getFields(data.getClass(), fields);

        // 遍历字段列表，将非空字段添加到SQL的set语句中
        fields.stream().forEach(field -> {
            // 获取字段的值
            Object valObj = getValue(field, data);
            if (valObj != null) sql.addSet(field.getName(), valObj);
        });

        // 解析SQL语句
        return sql.parse();
    }


    /**
     * 根据id更新数据SQL语句
     *
     * @param tableName 表名
     * @param data      数据
     * @param <T>       实体类
     * @return sql
     */
    public static <T> SQL<T> updateByIdSql(String tableName, T data) {
        // 创建SQL对象
        SQL<T> sql = SQL.<T>update().table(tableName);

        // 获取所有字段
        List<Field> fields = new ArrayList<>();
        getFields(data.getClass(), fields);

        // 遍历所有字段，将非id字段添加到set语句中
        fields.stream().filter(field -> !field.getName().equals("id")).forEach(field -> {
            Object valObj = getValue(field, data);
            if (valObj != null) sql.addSet(field.getName(), valObj);
        });

        // 获取id字段
        Field idField = fields.stream().filter(field -> field.getName().equals("id")).findAny().orElseThrow(() -> new ModelSqlUtilsException("id不存在"));

        // 获取id字段的值
        Object valObj = getValue(idField, data);
        if (valObj == null) throw new ModelSqlUtilsException("id值不能为空");

        // 添加where语句
        sql.addWhereEQ("id", valObj);

        return sql.parse();
    }


    /**
     * 根据id删除数据SQL语句
     *
     * @param tableName 表名
     * @param data      数据
     * @param <T>       实体类
     * @return SQL对象
     */
    public static <T> SQL<T> deleteByIdSql(String tableName, T data) {
        // 创建SQL对象
        SQL<T> sql = SQL.<T>delete().table(tableName);

        // 获取数据的字段列表
        List<Field> fields = new ArrayList<>();
        getFields(data.getClass(), fields);

        // 根据id字段名筛选出id字段
        Field idField = fields.stream().filter(field -> field.getName().equals("id")).findAny().orElseThrow(() -> new ModelSqlUtilsException("id不存在"));

        // 获取id字段的值
        Object valObj = getValue(idField, data);

        // 如果id值为空则抛出异常
        if (valObj == null) throw new ModelSqlUtilsException("id值不能为空");

        // 在sql语句中加入where条件，根据id值进行等值比较
        sql.addWhereEQ("id", valObj);

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
        // 创建SQL对象
        SQL<T> sql = (SQL<T>) SQL.select(data.getClass()).table(tableName);

        // 获取数据的字段列表
        List<Field> fields = new ArrayList<>();
        getFields(data.getClass(), fields);

        // 遍历字段列表，根据字段值生成where条件
        fields.forEach(field -> {
            Object valObj = getValue(field, data);
            if (valObj != null) sql.addWhereEQ(field.getName(), valObj);
        });

        // 解析并返回SQL
        return sql.parse();
    }


    /**
     * 根据给定的表名和实体class，生成创建表的SQL语句
     *
     * @param tableName 表名
     * @param clazz     实体class
     * @param <T>       实体类
     * @return 创建表的SQL语句
     */
    public static <T> String createSql(String tableName, Class<T> clazz) {
        // 获取实体类的所有字段
        List<Field> fields = new ArrayList<>();
        getFields(clazz, fields);

        // 构建SQL语句
        StringBuilder sb = new StringBuilder();
        sb.append("create table ").append(tableName).append(" (");
        fields.forEach(field -> sb.append(field.getName()).append(" ").append(getType(field)).append(","));
        int length = sb.length();
        sb.delete(length - 1, length).append(")");
        return sb.toString();
    }


    /**
     * 根据给定的表名和数据，生成对应的 SQL 语句。
     *
     * @param tableName 数据表的名称
     * @param data      数据对象
     * @return 生成的 SQL 语句
     */
    public static <T> String createSql(String tableName, T data) {
        // 调用 createSql 方法，传入表名和数据对象的 class
        return createSql(tableName, data.getClass());
    }

    /**
     * 根据给定的表名，生成删除表的SQL语句
     *
     * @param tableName 要删除的表名
     * @return 返回删除表的SQL语句
     */
    public static String dropSql(String tableName) {
        return "drop table " + tableName;
    }


}
