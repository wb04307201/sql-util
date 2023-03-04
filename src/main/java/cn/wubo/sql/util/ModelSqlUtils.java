package cn.wubo.sql.util;

import java.lang.reflect.Field;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Date;
import java.util.List;
import java.util.stream.Collectors;

public class ModelSqlUtils {

    private ModelSqlUtils(){}

    private static final String AND = " and ";

    /**
     * 反射获取类和父类的字段，并排除合成字段
     * @param clasz
     * @param fields
     */
    private static void getFields(Class<?> clasz, List<Field> fields) {
        if (clasz != null) {
            fields.addAll(Arrays.stream(clasz.getDeclaredFields()).filter(field -> !field.isSynthetic()).collect(Collectors.toList()));
            getFields(clasz.getSuperclass(), fields);
        }
    }

    /**
     * 对值的处理
     * @param field
     * @param data
     * @return
     * @param <T>
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
            throw new RuntimeException(e);
        }
    }

    /**
     * 转换数据库类型
     * @param field
     * @return
     */
    private static String getType(Field field) {
        if (field.getType().equals(Integer.class)) {
            return "int";
        } else {
            return "varchar2";
        }
    }

    /**
     * 插入数据sql
     * @param tableName 表名
     * @param data 数据
     * @return sql
     * @param <T> 实体类
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
     * @param tableName 表名
     * @param data 数据
     * @return sql
     * @param <T> 实体类
     */
    public static <T> String updateByIdSql(String tableName, T data) {
        List<Field> fields = new ArrayList<>();
        getFields(data.getClass(), fields);
        StringBuilder sb = new StringBuilder();
        sb.append("update ").append(tableName).append(" set ");
        fields.stream()
                .filter(field -> !field.getName().equals("id"))
                .forEach(field -> sb.append(field.getName()).append("=").append(getVaue(field, data)).append(","));
        int length = sb.length();
        sb.delete(length - 1, length);
        fields.stream()
                .filter(field -> field.getName().equals("id"))
                .findAny()
                .ifPresent(field -> sb.append(" where id=").append(getVaue(field, data)));
        return sb.toString();
    }

    /**
     * 根据id删除数据sql
     * @param tableName 表名
     * @param data 数据
     * @return sql
     * @param <T> 实体类
     */
    public static <T> String deleteByIdSql(String tableName, T data) {
        List<Field> fields = new ArrayList<>();
        getFields(data.getClass(), fields);
        StringBuilder sb = new StringBuilder();
        sb.append("delete from ").append(tableName);
        fields.stream()
                .filter(field -> field.getName().equals("id"))
                .findAny()
                .ifPresent(field -> sb.append(" where id=").append(getVaue(field, data)));
        return sb.toString();
    }

    /**
     * 查询数据sql
     * @param tableName 表名
     * @param data 数据
     * @return sql
     * @param <T> 实体类
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
     * @param tableName 表名
     * @param data 数据
     * @return sql
     * @param <T> 实体类
     */
    public static <T> String createSql(String tableName, T data) {
        List<Field> fields = new ArrayList<>();
        getFields(data.getClass(), fields);
        StringBuilder sb = new StringBuilder();
        sb.append("create table ").append(tableName).append(" (");
        fields.forEach(field -> sb.append(field.getName()).append(" ").append(getType(field)).append(","));
        int length = sb.length();
        sb.delete(length - 1, length).append(")");
        return sb.toString();
    }

    /**
     * 删除表sql
     * @param tableName 表名
     * @return sql
     */
    public static String dropSql(String tableName) {
        return "drop table " + tableName;
    }
}
