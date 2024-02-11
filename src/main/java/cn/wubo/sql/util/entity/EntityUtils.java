package cn.wubo.sql.util.entity;

import cn.wubo.sql.util.annotations.Table;
import cn.wubo.sql.util.cache.MemoryCache;
import cn.wubo.sql.util.exception.ModelSqlException;
import cn.wubo.sql.util.exception.TableModelException;
import cn.wubo.sql.util.utils.MapUtils;
import cn.wubo.sql.util.utils.StringUtils;

import java.lang.annotation.Annotation;
import java.lang.reflect.Field;
import java.lang.reflect.Modifier;
import java.math.BigDecimal;
import java.sql.Timestamp;
import java.text.ParseException;
import java.text.SimpleDateFormat;
import java.util.*;

public class EntityUtils {

    private EntityUtils() {
    }

    public static synchronized TableModel getTable(Class<?> clazz) {
        if (Boolean.TRUE.equals(MapUtils.isMap(clazz))) throw new TableModelException("不支持Map类型！");
        TableModel tableModel = MemoryCache.getTableModel(clazz);
        if (tableModel == null) {
            Annotation[] tableAnns = clazz.getAnnotations();
            Optional<Annotation> tableAnnOpt = Arrays.stream(tableAnns).filter(Table.class::isInstance).findAny();
            if (tableAnnOpt.isPresent()) {
                Table table = (Table) tableAnnOpt.get();
                tableModel = new TableModel(table.value(), StringUtils.defaultValue(table.desc(), clazz.getSimpleName())).setDs(table.ds());
            } else {
                tableModel = new TableModel(clazz.getSimpleName(), clazz.getSimpleName());
            }
            MemoryCache.putTableModel(clazz, tableModel.addColumns(transToColumns(clazz)));
            tableModel = MemoryCache.getTableModel(clazz);
        }
        return tableModel;
    }

    private static List<TableModel.ColumnModel> transToColumns(Class<?> clazz) {
        List<Field> fields = new ArrayList<>();
        getFields(clazz, fields);
        return fields.stream().map(TableModel.ColumnModel::new).sorted(Comparator.comparing(TableModel.ColumnModel::getSort)).toList();
    }

    private static void getFields(Class<?> clazz, List<Field> fields) {
        if (clazz != null) {
            fields.addAll(Arrays.stream(clazz.getDeclaredFields()).filter(field -> !field.isSynthetic()).filter(field -> !(Modifier.isFinal(field.getModifiers()) && Modifier.isStatic(field.getModifiers()))).toList());
            getFields(clazz.getSuperclass(), fields);
        }
    }

    /**
     * 获取字段的值
     *
     * @param field 字段
     * @param data  数据对象
     * @return 字段的值
     */
    public static <T> Object getValue(Field field, T data) {
        try {
            field.setAccessible(true);
            Object obj = field.get(data);
            if (obj != null) {
                if (obj instanceof Timestamp) return new SimpleDateFormat("yyyy-MM-dd HH:mm:ss.SSS").format(obj);
                else if (obj instanceof Date) return new SimpleDateFormat("yyyy-MM-dd").format(obj);
                else if (String.valueOf(obj).isEmpty()) return null;
                else return obj;
            } else {
                return null;
            }
        } catch (IllegalAccessException e) {
            throw new ModelSqlException(e);
        }
    }

    public static Object getValue(TableModel.ColumnModel col, Object obj) {
        if (obj != null && col.getField().getType() != obj.getClass()) {
            if (col.getField().getType() == Integer.class) return Integer.valueOf(obj.toString());
            else if (col.getField().getType() == Double.class) return Double.valueOf(obj.toString());
            else if (col.getField().getType() == Float.class) return Float.valueOf(obj.toString());
            else if (col.getField().getType() == BigDecimal.class) return new BigDecimal(obj.toString());
            else if (col.getField().getType() == Date.class) {
                try {
                    return new SimpleDateFormat("yyyy-MM-dd").parse(obj.toString());
                } catch (ParseException e) {
                    throw new ModelSqlException(e);
                }
            } else if (col.getField().getType() == Timestamp.class) {
                try {
                    return new SimpleDateFormat("yyyy-MM-dd HH:mm:ss.SSS").parse(obj.toString());
                } catch (ParseException e) {
                    throw new ModelSqlException(e);
                }
            } else return obj;
        } else {
            return obj;
        }
    }

}
