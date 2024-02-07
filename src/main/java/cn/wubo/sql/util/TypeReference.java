package cn.wubo.sql.util;

import cn.wubo.sql.util.cache.MemoryCache;

import java.lang.reflect.ParameterizedType;
import java.lang.reflect.Type;

public class TypeReference<T> {
    public final Class<T> clazz;

    protected TypeReference() {
        Type superClass = getClass().getGenericSuperclass();
        Type tempType = ((ParameterizedType) superClass).getActualTypeArguments()[0];
        Class<T> tempClazz = (Class<T>) MemoryCache.getClass(tempType);
        if (tempClazz == null) {
            if (tempType.toString().startsWith("class")) tempClazz = (Class<T>) tempType;
            else tempClazz = (Class<T>) ((ParameterizedType) tempType).getRawType();
            MemoryCache.putClass(tempType, tempClazz);
            tempClazz = (Class<T>) MemoryCache.getClass(tempType);
        }
        this.clazz = tempClazz;
    }
}
