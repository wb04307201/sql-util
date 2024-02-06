package cn.wubo.sql.util.entity;

import cn.wubo.sql.util.annotations.Table;
import cn.wubo.sql.util.exception.ModelSqlUtilsException;

import java.lang.annotation.Annotation;
import java.lang.reflect.Field;
import java.lang.reflect.Modifier;
import java.text.SimpleDateFormat;
import java.util.*;
import java.util.concurrent.ConcurrentHashMap;
import java.util.stream.Collectors;

public class EntityUtils {

    private EntityUtils() {
    }

    private static Map<String, TableModel> tableMap = new ConcurrentHashMap<>();

    public static synchronized Boolean check(String className) {
        return tableMap.containsKey(className);
    }

    public static synchronized TableModel getTable(String className) {
        return tableMap.get(className);
    }

    public static synchronized TableModel putTable(Class<?> clazz) {
        Annotation[] tableAnns = clazz.getAnnotations();
        Optional<Annotation> tableAnnOpt = Arrays.stream(tableAnns).filter(Table.class::isInstance).findAny();
        String tableName;
        String tableDesc;
        if (tableAnnOpt.isPresent()) {
            Table table = (Table) tableAnnOpt.get();
            tableName = table.value();
            tableDesc = table.desc();
        } else {
            tableName = clazz.getSimpleName();
            tableDesc = clazz.getSimpleName();
        }
        return tableMap.put(clazz.getName(), new TableModel(tableName, tableDesc).addColumns(transToColumns(clazz)));
    }

    private static List<TableModel.ColumnModel> transToColumns(Class<?> clazz) {
        List<Field> fields = new ArrayList<>();
        getFields(clazz, fields);
        return fields.stream().map(TableModel.ColumnModel::new).collect(Collectors.toList());
    }

    private static void getFields(Class<?> clazz, List<Field> fields) {
        if (clazz != null) {
            fields.addAll(Arrays.stream(clazz.getDeclaredFields()).filter(field -> !field.isSynthetic()).filter(field -> !(Modifier.isFinal(field.getModifiers()) && Modifier.isStatic(field.getModifiers()))).collect(Collectors.toList()));
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
    public static <T> Object getValue(Field field, T data) {
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
}
