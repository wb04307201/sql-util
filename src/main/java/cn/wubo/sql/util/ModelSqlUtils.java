package cn.wubo.sql.util;

import java.lang.reflect.Field;
import java.util.*;
import java.util.stream.Collectors;

public class ModelSqlUtils {

    private static void getFields(Class<?> clasz, List<Field> fields) {
        if (clasz != null) {
            fields.addAll(Arrays.stream(clasz.getDeclaredFields()).filter(field -> !field.isSynthetic()).collect(Collectors.toList()));
            getFields(clasz.getSuperclass(), fields);
        }
    }

    private static <T> String getVaue(Field field, T data) {
        try {
            field.setAccessible(true);
            Object obj = field.get(data);
            if (obj != null) {
                if (obj instanceof String) return "'" + obj + "'";
                else return String.valueOf(obj);
            } else {
                return null;
            }
        } catch (IllegalAccessException e) {
            throw new RuntimeException(e);
        }
    }

    private static String getType(Field field) {
        if (field.getType().equals(Integer.class)) {
            return "int";
        } else {
            return "varchar2";
        }
    }

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

    public static <T> String updateByIdSql(String tableName, T data) {
        List<Field> fields = new ArrayList<>();
        getFields(data.getClass(), fields);
        StringBuilder sb = new StringBuilder();
        sb.append("update ").append(tableName).append(" set ");
        fields.stream()
                .filter(field -> !field.getName().equals("id"))
                .forEach(field -> sb.append(field.getName()).append("=").append(getVaue(field, data)).append(","));
        int length = sb.length();
        sb.delete(length - 1, length).append(")");
        fields.stream()
                .filter(field -> field.getName().equals("id"))
                .findAny()
                .ifPresent(field -> sb.append(" where id=").append(getVaue(field, data)));
        return sb.toString();
    }

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

    public static <T> String selectSql(String tableName, T data) {
        List<Field> fields = new ArrayList<>();
        getFields(data.getClass(), fields);
        StringBuilder sb = new StringBuilder();
        sb.append("select * from ").append(tableName);
        fields.forEach(field -> {
            String strValue = getVaue(field, data);
            if (strValue != null) sb.append(" and ").append(field.getName()).append("=").append(getVaue(field, data));
        });
        String str = sb.toString();
        return str.indexOf("and") > 0 ? str.replaceFirst("and", "where") : str;
    }

    public static <T> String createSql(String tableName, T data) {
        List<Field> fields = new ArrayList<>();
        getFields(data.getClass(), fields);
        StringBuilder sb = new StringBuilder();
        sb.append("create table ").append(tableName).append(" (");
        fields.forEach(field -> {
            field.getType().equals(String.class);
            sb.append(field.getName()).append(" ").append(getType(field)).append(",");
        });
        int length = sb.length();
        sb.delete(length - 1, length).append(")");
        return sb.toString();
    }

    public static String dropSql(String tableName) {
        return "drop table " + tableName;
    }
}
