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
import java.util.stream.Collectors;

public class EntityUtils {

    private EntityUtils() {
    }

    /**
     * 获取表模型
     * @param clazz 类型
     * @return 表模型
     * @throws TableModelException 如果类型为Map类型则抛出异常
     */
    public static synchronized TableModel getTable(Class<?> clazz) throws TableModelException {
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

    /**
     * 将给定的类转换为列模型列表
     *
     * @param clazz 类对象
     * @return 列模型列表
     */
    private static List<TableModel.ColumnModel> transToColumns(Class<?> clazz) {
        List<Field> fields = new ArrayList<>();
        getFields(clazz, fields);
        return fields.stream().map(TableModel.ColumnModel::new).sorted(Comparator.comparing(TableModel.ColumnModel::getSort)).collect(Collectors.toList());
    }

    /**
     * 获取指定类及其父类的所有非静态非final字段
     * @param clazz 指定的类
     * @param fields 存储字段的列表
     */
    private static void getFields(Class<?> clazz, List<Field> fields) {
        if (clazz != null) {
            // 获取指定类的所有非静态非final字段
            fields.addAll(Arrays.stream(clazz.getDeclaredFields()).filter(field -> !field.isSynthetic()).filter(field -> !(Modifier.isFinal(field.getModifiers()) && Modifier.isStatic(field.getModifiers()))).collect(Collectors.toList()));
            // 递归获取父类的所有非静态非final字段
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
                // 如果字段的值不为空
                if (String.valueOf(obj).isEmpty()) return null;
                else if (obj instanceof Timestamp) return new SimpleDateFormat("yyyy-MM-dd HH:mm:ss.SSS").format(obj);
                else if (obj instanceof Date) return new SimpleDateFormat("yyyy-MM-dd").format(obj);
                else if (obj instanceof Float || obj instanceof Double) return subZeroAndDot(String.valueOf(obj));
                else return obj;
            } else {
                // 如果字段的值为空
                return null;
            }
        } catch (IllegalAccessException e) {
            throw new ModelSqlException(e);
        }
    }

    /**
     * 去掉字符串中多余的0和小数点
     *
     * @param s 待处理的字符串
     * @return 处理后的字符串
     */
    public static String subZeroAndDot(String s) {
        if (s.contains(".")) {
            s = s.replaceAll("0+$", "");//去掉多余的0
            s = s.replaceAll("[.]$", "");//如最后一位是.则去掉
        }
        return s;
    }

    /**
     * 获取指定列的值
     * @param col 列模型
     * @param obj 对象
     * @return 列的值
     */
    public static Object getValue(TableModel.ColumnModel col, Object obj) {
        if (obj != null && col.getField().getType() != obj.getClass()) {
            // 如果对象的类型与列的类型不一致，则进行类型转换
            if (col.getField().getType() == Integer.class) return Integer.valueOf(obj.toString());
            else if (col.getField().getType() == Double.class) return Double.valueOf(obj.toString());
            else if (col.getField().getType() == Float.class) return Float.valueOf(obj.toString());
            else if (col.getField().getType() == BigDecimal.class) return new BigDecimal(obj.toString());
            else if (col.getField().getType() == Date.class) {
                try {
                    // 将字符串转换为日期对象
                    return new SimpleDateFormat("yyyy-MM-dd").parse(obj.toString());
                } catch (ParseException e) {
                    throw new ModelSqlException(e);
                }
            } else if (col.getField().getType() == Timestamp.class) {
                try {
                    // 将字符串转换为时间戳对象
                    return new SimpleDateFormat("yyyy-MM-dd HH:mm:ss.SSS").parse(obj.toString());
                } catch (ParseException e) {
                    throw new ModelSqlException(e);
                }
            } else return obj;
        } else {
            // 如果对象的类型与列的类型一致，则直接返回对象
            return obj;
        }
    }

}
