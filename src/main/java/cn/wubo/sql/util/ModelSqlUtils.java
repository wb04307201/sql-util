package cn.wubo.sql.util;

import cn.wubo.sql.util.exception.ModelSqlUtilsException;

import java.lang.reflect.Field;
import java.lang.reflect.Modifier;
import java.math.BigDecimal;
import java.sql.Blob;
import java.sql.Clob;
import java.sql.Time;
import java.sql.Timestamp;
import java.util.*;
import java.util.stream.Collectors;

/**
 * 根据实体类生成sql
 */
public class ModelSqlUtils {

    private ModelSqlUtils() {
    }

    private static final String AND = " and ";

    /**
     * 反射获取类和父类的字段
     * 排除合成字段
     * 排除static final
     *
     * @param clasz  类
     * @param fields Field集合
     */
    private static void getFields(Class<?> clasz, List<Field> fields) {
        if (clasz != null) {
            fields.addAll(Arrays.stream(clasz.getDeclaredFields()).filter(field -> !field.isSynthetic()).filter(field -> !(Modifier.isFinal(field.getModifiers()) && Modifier.isStatic(field.getModifiers()))).collect(Collectors.toList()));
            getFields(clasz.getSuperclass(), fields);
        }
    }

    /**
     * 对值的处理
     *
     * @param field
     * @param data
     * @param <T>
     * @return
     */
    private static <T> String getVaue(Field field, T data) {
        try {
            field.setAccessible(true);
            Object obj = field.get(data);
            if (obj != null) {
                if (obj instanceof String) return "'" + obj + "'";
                if (obj instanceof Date) return "'" + obj + "'";
                else return String.valueOf(obj);
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
     * @param field
     * @return
     */
    private static String getType(Field field) {
        if (field.getType().equals(Integer.class)) {
            return "int";
        } else if (field.getType().equals(Long.class)) {
            return "bigint";
        } else if (field.getType().equals(Float.class)) {
            return "float";
        } else if (field.getType().equals(Double.class)) {
            return "double";
        } else if (field.getType().equals(BigDecimal.class)) {
            return "numeric";
        } else if (field.getType().equals(java.util.Date.class) || field.getType().equals(java.sql.Date.class)) {
            return "date";
        } else if (field.getType().equals(Time.class)) {
            return "time";
        } else if (field.getType().equals(Timestamp.class) || field.getType().equals(Calendar.class)) {
            return "timestamp";
        } else if (field.getType().equals(Boolean.class)) {
            return "bit";
        } else if (field.getType().equals(Blob.class)) {
            return "blob";
        } else if (field.getType().equals(Clob.class)) {
            return "clob";
        } else {
            return "varchar2";
        }
    }

    /**
     * 插入数据sql
     *
     * @param tableName 表名
     * @param data      数据
     * @param <T>       实体类
     * @return sql
     */
    public static <T> String insertSql(String tableName, T data) {
        List<Field> fields = new ArrayList<>();
        getFields(data.getClass(), fields);
        StringBuilder sb = new StringBuilder();
        sb.append("insert into ").append(tableName).append(" (");
        fields.forEach(field -> sb.append(field.getName()).append(","));
        int length = sb.length();
        sb.delete(length - 1, length).append(") values (");
        fields.forEach(field -> sb.append(getVaue(field, data)).append(","));
        length = sb.length();
        sb.delete(length - 1, length).append(")");
        return sb.toString();
    }

    /**
     * 根据id更新数据sql
     *
     * @param tableName 表名
     * @param data      数据
     * @param <T>       实体类
     * @return sql
     */
    public static <T> String updateByIdSql(String tableName, T data) {
        List<Field> fields = new ArrayList<>();
        getFields(data.getClass(), fields);
        StringBuilder sb = new StringBuilder();
        sb.append("update ").append(tableName).append(" set ");
        fields.stream().filter(field -> !field.getName().equals("id")).forEach(field -> sb.append(field.getName()).append("=").append(getVaue(field, data)).append(","));
        int length = sb.length();
        sb.delete(length - 1, length);
        Optional<Field> optional = fields.stream().filter(field -> field.getName().equals("id")).findAny();
        if (optional.isPresent()) {
            String value = getVaue(optional.get(), data);
            if (value != null) return sb.append(" where id=").append(value).toString();
        }
        throw new ModelSqlUtilsException("id值不能为空");
    }

    /**
     * 根据id删除数据sql
     *
     * @param tableName 表名
     * @param data      数据
     * @param <T>       实体类
     * @return sql
     */
    public static <T> String deleteByIdSql(String tableName, T data) {
        List<Field> fields = new ArrayList<>();
        getFields(data.getClass(), fields);
        StringBuilder sb = new StringBuilder();
        sb.append("delete from ").append(tableName);
        Optional<Field> optional = fields.stream().filter(field -> field.getName().equals("id")).findAny();
        if (optional.isPresent()) {
            String value = getVaue(optional.get(), data);
            if (value != null) return sb.append(" where id=").append(value).toString();
        }
        throw new ModelSqlUtilsException("id值不能为空");
    }

    /**
     * 查询数据sql
     *
     * @param tableName 表名
     * @param data      数据
     * @param <T>       实体类
     * @return sql
     */
    public static <T> String selectSql(String tableName, T data) {
        List<Field> fields = new ArrayList<>();
        getFields(data.getClass(), fields);
        StringBuilder sb = new StringBuilder();
        sb.append("select * from ").append(tableName);
        fields.forEach(field -> {
            String strValue = getVaue(field, data);
            if (strValue != null) sb.append(AND).append(field.getName()).append("=").append(getVaue(field, data));
        });
        String str = sb.toString();
        return str.contains(AND) ? str.replaceFirst(AND, " where ") : str;
    }

    /**
     * 创建表sql
     *
     * @param tableName 表名
     * @param clazz     实体class
     * @param <T>       实体类
     * @return sql
     */
    public static <T> String createSql(String tableName, Class<T> clazz) {
        List<Field> fields = new ArrayList<>();
        getFields(clazz, fields);
        StringBuilder sb = new StringBuilder();
        sb.append("create table ").append(tableName).append(" (");
        fields.forEach(field -> sb.append(field.getName()).append(" ").append(getType(field)).append(","));
        int length = sb.length();
        sb.delete(length - 1, length).append(")");
        return sb.toString();
    }

    public static <T> String createSql(String tableName, T data) {
        return createSql(tableName, data.getClass());
    }

    /**
     * 删除表sql
     *
     * @param tableName 表名
     * @return sql
     */
    public static String dropSql(String tableName) {
        return "drop table " + tableName;
    }
}
