package cn.wubo.sql.util.cache;

import cn.wubo.sql.util.entity.TableModel;

import java.lang.reflect.Type;
import java.util.concurrent.ConcurrentHashMap;
import java.util.concurrent.ConcurrentMap;

public class MemoryCache {

    private MemoryCache() {
    }

    private static final ConcurrentMap<Type, Class<?>> classCache = new ConcurrentHashMap<>(16, 0.75f, 1);

    public static Class<?> getClass(Type type) {
        return classCache.get(type);
    }

    public static void putClass(Type type, Class<?> clazz) {
        classCache.putIfAbsent(type, clazz);
    }

    private static final ConcurrentMap<Class<?>, TableModel> tableModelCache = new ConcurrentHashMap<>(16, 0.75f, 1);

    public static TableModel getTableModel(Class<?> clazz) {
        return tableModelCache.get(clazz);
    }

    public static void putTableModel(Class<?> clazz, TableModel tableModel) {
        tableModelCache.putIfAbsent(clazz, tableModel);
    }
}
