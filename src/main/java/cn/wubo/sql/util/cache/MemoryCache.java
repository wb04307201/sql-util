package cn.wubo.sql.util.cache;

import cn.wubo.sql.util.entity.TableModel;

import java.lang.reflect.Type;
import java.util.concurrent.ConcurrentHashMap;
import java.util.concurrent.ConcurrentMap;

/**
 * 提供基于内存的缓存服务，主要实现了对类型与类对象、类与TableModel对象之间的快速查找与存储功能。
 */
public class MemoryCache {

    /**
     * 私有化构造方法，防止外部实例化该类。
     */
    private MemoryCache() {
    }

    /**
     * 缓存类型与对应的类对象，使用ConcurrentHashMap保证线程安全。
     */
    private static final ConcurrentMap<Type, Class<?>> classCache = new ConcurrentHashMap<>(16, 0.75f, 1);

    /**
     * 根据给定的类型从缓存中获取对应的类对象。
     *
     * @param type 需要查找的类型。
     * @return 类型对应的类对象，若未找到则返回null。
     */
    public static Class<?> getClass(Type type) {
        return classCache.get(type);
    }

    /**
     * 将指定的类型与类对象关联并存入缓存中。如果类型与类对象的对应关系已存在，则不会覆盖。
     *
     * @param type   待缓存的类型。
     * @param clazz  对应的类对象。
     */
    public static void putClass(Type type, Class<?> clazz) {
        classCache.putIfAbsent(type, clazz);
    }

    /**
     * 缓存类对象与对应的TableModel对象，使用ConcurrentHashMap保证线程安全。
     */
    private static final ConcurrentMap<Class<?>, TableModel> tableModelCache = new ConcurrentHashMap<>(16, 0.75f, 1);

    /**
     * 根据给定的类对象从缓存中获取对应的TableModel对象。
     *
     * @param clazz 需要查找的类对象。
     * @return 类对象对应的TableModel对象，若未找到则返回null。
     */
    public static TableModel getTableModel(Class<?> clazz) {
        return tableModelCache.get(clazz);
    }

    /**
     * 将指定的类对象与TableModel对象关联并存入缓存中。如果类对象与TableModel对象的对应关系已存在，则不会覆盖。
     *
     * @param clazz      待缓存的类对象。
     * @param tableModel 对应的TableModel对象。
     */
    public static void putTableModel(Class<?> clazz, TableModel tableModel) {
        tableModelCache.putIfAbsent(clazz, tableModel);
    }
}

