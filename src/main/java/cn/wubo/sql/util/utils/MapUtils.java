package cn.wubo.sql.util.utils;

import java.util.*;
import java.util.concurrent.ConcurrentHashMap;
import java.util.concurrent.ConcurrentMap;

public class MapUtils {

    private static final String MAP_NAME = "java.util.Map";

    private MapUtils() {
    }

    /**
     * 判断给定的类是否是Map接口或其实现类
     * @param clazz 给定的类
     * @return 如果是Map接口或其实现类则返回true，否则返回false
     */
    public static Boolean isMap(Class<?> clazz) {
        // 判断给定的类是否是Map接口或其实现类
        return clazz.getName().equals(MAP_NAME) || Arrays.stream(clazz.getInterfaces()).anyMatch(item -> item.getName().equals(MAP_NAME)) || Arrays.stream(clazz.getSuperclass().getInterfaces()).anyMatch(item -> item.getName().equals(MAP_NAME));
    }

    /**
     * 根据给定的类类型创建并返回一个空的Map对象。
     *
     * @param clazz Map对象的类类型
     * @return 创建的空的Map对象
     * @throws InstantiationException 当实例化创建Map对象时发生异常
     * @throws IllegalAccessException 当访问实例化创建的Map对象时发生异常
     */
    public static <T> T createMap(Class<T> clazz) throws InstantiationException, IllegalAccessException {
        if (clazz == Properties.class) return (T) new Properties();
        if (clazz == Hashtable.class) return (T) new Hashtable<>();
        if (clazz == IdentityHashMap.class) return (T) new IdentityHashMap<>();
        if (clazz == SortedMap.class || clazz == TreeMap.class) return (T) new TreeMap<>();
        if (clazz == ConcurrentMap.class || clazz == ConcurrentHashMap.class) return (T) new ConcurrentHashMap<>();
        if (clazz == Map.class || clazz == HashMap.class) return (T) new HashMap<>();
        if (clazz == LinkedHashMap.class) return (T) new LinkedHashMap<>();
        return clazz.newInstance();
    }
}
