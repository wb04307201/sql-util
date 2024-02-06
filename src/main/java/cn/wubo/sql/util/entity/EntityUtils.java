package cn.wubo.sql.util.entity;

import cn.wubo.sql.util.annotations.Table;

import java.lang.annotation.Annotation;
import java.lang.reflect.Field;
import java.lang.reflect.Modifier;
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
        fields.addAll(Arrays.stream(clazz.getDeclaredFields()).filter(field -> !field.isSynthetic()).filter(field -> !(Modifier.isFinal(field.getModifiers()) && Modifier.isStatic(field.getModifiers()))).collect(Collectors.toList()));
        getFields(clazz.getSuperclass(), fields);
    }
}
