package cn.wubo.sql.util;

import cn.wubo.sql.util.cache.MemoryCache;

import java.lang.reflect.ParameterizedType;
import java.lang.reflect.Type;

public class TypeReference<T> {
    public final Class<T> clazz;

    /**
     * 保护类型的构造函数，用于初始化TypeReference类的实例。
     * 该构造函数通过分析当前对象的泛型超类，来初始化本类的泛型类型。
     * 在这个过程中，它会检查并更新一个内存缓存（MemoryCache），以确保泛型类型的正确缓存。
     */
    protected TypeReference() {
        // 获取当前对象的泛型超类
        Type superClass = getClass().getGenericSuperclass();
        // 获取泛型超类的第一个实际类型参数
        Type tempType = ((ParameterizedType) superClass).getActualTypeArguments()[0];
        // 尝试从内存缓存中获取对应类型
        Class<T> memClazz = (Class<T>) MemoryCache.getClass(tempType);
        Class<T> tempClazz;
        // 根据类型的不同（原始类型或参数化类型），获取对应的Class对象
        if (tempType.toString().startsWith("class")) tempClazz = (Class<T>) tempType;
        else tempClazz = (Class<T>) ((ParameterizedType) tempType).getRawType();
        // 如果缓存中的类与实际需要的类不一致，则更新缓存并重新获取
        if (!tempClazz.equals(memClazz)) {
            MemoryCache.putClass(tempType, tempClazz);
            tempClazz = (Class<T>) MemoryCache.getClass(tempType);
        }
        // 将获取到的类赋值给clazz字段
        this.clazz = tempClazz;
    }
}
