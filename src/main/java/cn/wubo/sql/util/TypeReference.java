package cn.wubo.sql.util;

import java.lang.reflect.ParameterizedType;
import java.lang.reflect.Type;
import java.util.concurrent.ConcurrentHashMap;
import java.util.concurrent.ConcurrentMap;

public class TypeReference<T> {
    private static ConcurrentMap<Type, Type> classTypeCache = new ConcurrentHashMap<>(16, 0.75f, 1);
    public final Type type;

    protected TypeReference() {
        Type superClass = getClass().getGenericSuperclass();
        Type tempType = ((ParameterizedType) superClass).getActualTypeArguments()[0];
        Type cachedType = classTypeCache.get(tempType);
        if (cachedType == null) {
            classTypeCache.putIfAbsent(tempType, tempType);
            cachedType = classTypeCache.get(tempType);
        }
        this.type = cachedType;
    }
}
