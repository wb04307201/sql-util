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

    /**
     * 获取指定类的表模型。该表模型用于描述数据库表的结构和属性。
     *
     * @param clazz 需要获取表模型的类。该类必须不是Map类型。
     * @return 表模型对象，包含了类与数据库表的映射信息。
     * @throws TableModelException 如果传入的类是Map类型，则抛出此异常。
     */
    public static synchronized TableModel getTable(Class<?> clazz) throws TableModelException {
        // 检查类是否为Map类型，如果是则抛出异常
        if (Boolean.TRUE.equals(MapUtils.isMap(clazz))) throw new TableModelException("不支持Map类型！");

        // 尝试从缓存中获取表模型，如果未命中则进行创建和缓存
        TableModel tableModel = MemoryCache.getTableModel(clazz);
        if (tableModel == null) {
            // 获取类上的@Table注解，用于提取表名和描述等信息
            Annotation[] tableAnns = clazz.getAnnotations();
            Optional<Annotation> tableAnnOpt = Arrays.stream(tableAnns).filter(Table.class::isInstance).findAny();

            if (tableAnnOpt.isPresent()) {
                // 如果存在@Table注解，解析注解信息创建表模型
                Table table = (Table) tableAnnOpt.get();
                tableModel = new TableModel(table.value(), StringUtils.defaultValue(table.desc(), clazz.getSimpleName()), table.init()).setDs(table.ds());
            } else {
                // 如果没有@Table注解，使用类名作为默认表名和描述
                tableModel = new TableModel(clazz.getSimpleName(), clazz.getSimpleName(), true);
            }

            // 将新创建的表模型缓存起来
            MemoryCache.putTableModel(clazz, tableModel.addColumns(transToColumns(clazz)));
            // 重新从缓存中获取表模型，以确保后续操作使用的是缓存中的对象
            tableModel = MemoryCache.getTableModel(clazz);
        }
        return tableModel;
    }

    /**
     * 将给定的类转换为列模型列表
     *
     * 该方法通过反射获取类中的所有字段，并将每个字段转换为一个列模型（TableModel.ColumnModel）。
     * 列模型列表是根据字段的排序属性（getSort）进行排序的。
     *
     * @param clazz 类对象。要转换为列模型列表的类。
     * @return 列模型列表。包含类中所有字段的列表，每个字段都作为一个列模型表示。
     */
    private static List<TableModel.ColumnModel> transToColumns(Class<?> clazz) {
        // 初始化一个空列表，用于存放从类中获取到的字段
        List<Field> fields = new ArrayList<>();
        // 通过递归获取类及其父类中所有的非静态字段
        getFields(clazz, fields);
        // 将字段列表转换为列模型列表，并根据列模型的排序属性进行排序
        return fields.stream().map(TableModel.ColumnModel::new).sorted(Comparator.comparing(TableModel.ColumnModel::getSort)).toList();
    }

    /**
     * 获取指定类及其父类的所有非静态非final字段
     *
     * @param clazz 指定的类，从该类开始向上递归查找非静态非final字段
     * @param fields 存储找到的字段的列表，会将该类及其父类中满足条件的字段添加到此列表中
     */
    private static void getFields(Class<?> clazz, List<Field> fields) {
        if (clazz != null) {
            // 获取当前类中所有非静态非final字段
            fields.addAll(Arrays.stream(clazz.getDeclaredFields()).filter(field -> !field.isSynthetic()).filter(field -> !(Modifier.isFinal(field.getModifiers()) && Modifier.isStatic(field.getModifiers()))).toList());
            // 递归处理父类，以获取父类及其父类中的非静态非final字段
            getFields(clazz.getSuperclass(), fields);
        }
    }

    /**
     * 获取指定对象中字段的值。
     * 该方法会尝试访问对象中的指定字段，并根据字段的类型对值进行格式化处理。
     * 如果字段值为空或格式化失败，则返回null。
     *
     * @param field 要获取值的字段
     * @param data 包含字段的 数据对象
     * @return 字段的值，经过适当格式化处理
     * @throws ModelSqlException 如果访问字段时发生异常
     */
    public static <T> Object getValue(Field field, T data) {
        try {
            field.setAccessible(true);  // 允许访问私有字段
            Object obj = field.get(data); // 获取字段的值
            if (obj != null) {
                // 根据字段值的类型进行不同的格式化处理
                if (String.valueOf(obj).isEmpty()) return null;
                else if (obj instanceof Timestamp) return new SimpleDateFormat("yyyy-MM-dd HH:mm:ss.SSS").format(obj);
                else if (obj instanceof Date) return new SimpleDateFormat("yyyy-MM-dd").format(obj);
                else if (obj instanceof Float || obj instanceof Double) return subZeroAndDot(String.valueOf(obj));
                else return obj;
            } else {
                // 字段值为空时返回null
                return null;
            }
        } catch (IllegalAccessException e) {
            // 访问字段发生异常时，抛出ModelSqlException
            throw new ModelSqlException(e);
        }
    }

    /**
     * 去掉字符串中多余的0和小数点
     * 该方法接收一个字符串参数，并返回处理后的字符串。处理规则如下：
     * 如果字符串包含小数点，则移除小数点后所有的0，并且如果小数点是字符串的最后一位，也将其移除。
     *
     * @param s 待处理的字符串
     * @return 处理后的字符串，移除了多余的0和小数点
     */
    public static String subZeroAndDot(String s) {
        // 如果字符串包含小数点，进行处理
        if (s.contains(".")) {
            // 移除小数点后所有的0
            s = s.replaceAll("0+$", "");
            // 移除小数点如果是字符串的最后一位
            s = s.replaceAll("[.]$", "");
        }
        return s;
    }

    /**
     * 获取指定列的值。此方法用于从对象中获取对应列的值，并处理类型不匹配的情况。如果对象类型与列类型不一致，会尝试进行类型转换。
     *
     * @param col 列模型，包含列的相关信息，如字段类型。
     * @param obj 需要获取值的对象。
     * @return 列的值，经过类型转换后与列类型一致。
     */
    public static Object getValue(TableModel.ColumnModel col, Object obj) {
        if (obj != null && col.getField().getType() != obj.getClass()) {
            // 对象类型与列类型不一致时的处理逻辑
            if (col.getField().getType() == Integer.class) return Integer.valueOf(subZeroAndDot(obj.toString()));
            else if (col.getField().getType() == Long.class) return Long.valueOf(subZeroAndDot(obj.toString()));
            else if (col.getField().getType() == Double.class) return Double.valueOf(obj.toString());
            else if (col.getField().getType() == Float.class) return Float.valueOf(obj.toString());
            else if (col.getField().getType() == BigDecimal.class) return new BigDecimal(obj.toString());
            else if (col.getField().getType() == Date.class) {
                try {
                    // 字符串转日期
                    return new SimpleDateFormat("yyyy-MM-dd").parse(obj.toString());
                } catch (ParseException e) {
                    throw new ModelSqlException(e);
                }
            } else if (col.getField().getType() == Timestamp.class) {
                try {
                    // 字符串转时间戳
                    return new SimpleDateFormat("yyyy-MM-dd HH:mm:ss.SSS").parse(obj.toString());
                } catch (ParseException e) {
                    throw new ModelSqlException(e);
                }
            } else return obj;
        } else {
            // 对象类型与列类型一致时，直接返回对象
            return obj;
        }
    }

}
